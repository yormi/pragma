module TypeCheck.Check
    ( Check
    , Context(..)
    , TypeError(..)
    , fail
    , initialContext
    , lookupVariable
    , variablesInContext
    , run
    , runWithContext
    , showResult
    , unifyTypeVariable
    , untypedVariable
    , withVariables
    ) where


import Control.Monad.Trans.Class (lift)
import Control.Monad.Reader (ReaderT, runReaderT)
import qualified Control.Monad.Reader as Reader
import qualified Control.Monad.State as State
import Control.Monad.State (StateT, evalStateT)
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty)

import qualified AST.Expression as E
import qualified Type as T
import qualified Utils.Either as Either


-- TODO Change type... currently an error won't cumulate errors
type Check a =
    ReaderT
        Context
        (StateT State
            (Either TypeError)
        )
        a


data State =
    State
        { typeVariableCounter :: Int
        , constraints :: [Constraint]
        }
        deriving (Eq, Show)


data Constraint
    = BoundWith T.VariableNumber T.VariableNumber
    | ResolvedTo T.VariableNumber T.Type
        deriving (Eq, Show)


data Context =
    Context
        { variables :: [Variable]
        , typeVariable :: [TypeVariable]
        }
        deriving (Eq, Show)


type Variable = (E.Identifier, T.Type)

type TypeVariable = (Int, T.Type)


initialState :: State
initialState =
    State 0 []


initialContext :: Context
initialContext =
    Context [] []


data TypeError
    = Mismatch
        { expected :: T.Type
        , actual :: T.Type
        }
    | FunctionBodyMismatch
        { returningType :: T.Type
        , bodyType :: T.Type
        }
    | TooManyParam
        { functionType :: T.Type
        , params :: [E.Identifier]
        }
    | NotFunction T.Type
    | NotInScope E.Identifier
    | IfConditionMustBeBool E.Expr
    | BothIfExpressionsMustHaveSameType E.Expr E.Expr
    | PatternMismatch E.Expr [E.Pattern]
    | AllCasesMustHaveSameType (NonEmpty E.Expr)
    | ShadowingIds [E.Identifier]
    | TODO E.Expr
    | TooManyArguments
        { functionType :: T.Type
        , arguments :: NonEmpty E.Argument
        }
    | WrongArgumentType
        { expected :: T.Type
        , actual :: T.Type
        , argument :: E.Argument
        }
    | ImpossibleConstraints
    deriving (Eq, Show)


fail :: TypeError -> Check a
fail =
    Left >> lift >> lift


variablesInContext :: Check [Variable]
variablesInContext =
    Reader.ask
        |> map variables


withVariables :: [( E.Identifier, T.Type )] -> Check T.Type -> Check T.Type
withVariables newVariables innerChecker = do
    currentVariables <- variablesInContext
    let currentIdentifiers = map fst currentVariables
    let shadowingIds =
            newVariables
                |> map fst
                |> List.intersect currentIdentifiers

    case shadowingIds of
        [] ->
            let
                addToContext c =
                    c { variables = newVariables ++ (variables c) }
            in
            Reader.local addToContext innerChecker

        ids ->
            fail <| ShadowingIds ids


untypedVariable :: E.Identifier -> Check Variable
untypedVariable id = do
    state <- lift State.get
    let type_ = T.Variable <| typeVariableCounter state
    State.modify (\s -> s { typeVariableCounter = typeVariableCounter s + 1 })
        |> lift
    return (id, type_)


unifyTypeVariable :: Int -> T.Type -> Check ()
unifyTypeVariable typeVariableNumber constrainingType =
    let
        isBoundAlreadyExist state n =
            List.any
                (\c ->
                    c == BoundWith typeVariableNumber n
                        || c == BoundWith n typeVariableNumber
                )
                (constraints state)


        hasAlreadyBeenResolved state =
            List.any
                (\c ->
                    case c of
                        ResolvedTo n _ ->
                            n == typeVariableNumber

                        _ ->
                            False
                )
                (constraints state)
    in do
    state <- lift State.get
    case constrainingType of
        T.Variable n ->
            if isBoundAlreadyExist state n then
                return ()

            else
                BoundWith typeVariableNumber n
                    |> (\c -> state { constraints = c : (constraints state) })
                    |> State.put
                    |> lift

        _ ->
            if hasAlreadyBeenResolved state then
                fail ImpossibleConstraints

            else
                ResolvedTo typeVariableNumber constrainingType
                    |> (\c -> state { constraints = c : (constraints state) })
                    |> State.put
                    |> lift



lookupVariable :: E.Identifier -> Check T.Type
lookupVariable id = do
  vs <- variablesInContext
  case List.lookup id vs of
    Just value  ->
        return value

    Nothing ->
        fail <| NotInScope id


showResult :: [Check T.Type] -> String
showResult =
    map run
        >> map (Either.fold (\e -> "FAIL - " ++ show e) show)
        >> List.intercalate "\n"


run :: Check T.Type -> Either TypeError T.Type
run =
    flip runReaderT initialContext
        >> (flip evalStateT initialState)


runWithContext :: Context -> Check T.Type -> Either TypeError T.Type
runWithContext context =
    flip runReaderT context
        >> (flip evalStateT initialState)
