module TypeChecker
    ( check
    , showCheckResult
    ) where


import Control.Monad.Trans.Class (lift)
import Control.Monad.Reader (ReaderT, runReaderT)
import qualified Control.Monad.Reader as Reader
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Tuple as Tuple

import qualified AST.Expression as E
import qualified AST.Module as M
import AST.Module (Module)
import qualified Type as T
import qualified Utils.Either as Either


-- TODO Change type... currently an error won't cumulate errors
type Check = ReaderT Env (Either TypeError) T.Type


type Env = [(E.Identifier, T.Type)]


data TypeError
    = Mismatch
        { expected :: T.Type
        , actual :: T.Type
        }
    | FunctionBodyMismatch
        { env :: Env
        , returningType :: T.Type
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
    | ShadowingIds E.Expr [E.Identifier]
    | TODO E.Expr
    deriving (Eq, Show)


failCheck :: TypeError -> Check
failCheck =
    Left >> lift


-- TODO Clean up that expression param stuff
-- TODO Pretty print expression
-- TODO Add position in AST element
withEnv :: [( E.Identifier, T.Type )] -> E.Expr -> Check -> Check
withEnv references expression innerChecker = do
    currentEnv <- Reader.ask
    let currentIdentifiers = map fst currentEnv
    let shadowingIds =
            references
                |> map fst
                |> List.intersect currentIdentifiers

    case shadowingIds of
        [] ->
            Reader.local
                (\env -> references ++ env)
                innerChecker

        ids ->
            failCheck <| ShadowingIds expression ids





lookupReference :: E.Identifier -> Check
lookupReference id = do
  env <- Reader.ask
  case List.lookup id env of
    Just value  ->
        return value

    Nothing ->
        failCheck <| NotInScope id


--


showCheckResult :: [Check] -> String
showCheckResult =
    map (flip runReaderT [])
        >> map (Either.fold (\e -> "FAIL - " ++ show e) show)
        >> List.intercalate "\n"


check :: Module -> [Check]
check (M.Module topLevels) =
    topLevels
        |> map (\topLevel ->
            case topLevel of
                M.Function { M.type_, M.params, M.body } -> do
                    case type_ of
                        Just t -> do
                            functionType <- returningFunctionType t params

                            currentEnv <- Reader.ask

                            let ps = paramsWithTypes t params
                            expressionType  <-
                                withEnv ps body <| checkExpr body

                            if functionType == expressionType then
                                return functionType

                            else
                                FunctionBodyMismatch
                                    (ps ++ currentEnv)
                                    functionType expressionType
                                    |> failCheck

                        Nothing ->
                            checkExpr body
        )


-- TODO Shadowing
-- TODO Merge with returningFunctionType
paramsWithTypes :: T.Type -> [E.Identifier] -> [( E.Identifier, T.Type )]
paramsWithTypes functionType functionParams =
    functionParams
        |> List.foldl
            (\(currentType, params) paramId -> do
                case currentType of
                    T.Function a b ->
                        (paramId, a) : params
                            |> (\newEnv -> (b, newEnv))

                    _ ->
                        (currentType, [])
            )
            (functionType, [])
        |> Tuple.snd


returningFunctionType :: T.Type -> [E.Identifier] -> Check
returningFunctionType type_ params =
    List.foldl
        (\result _ -> do
            currentType <- result

            case currentType of
                T.Function _ returnValue ->
                    return returnValue

                _ ->
                    failCheck <| TooManyParam type_ params
        )
        (return type_)
        params


checkExpr :: E.Expr -> Check
checkExpr expr =
    case expr of
        E.Value v ->
            checkValue v

        E.Reference r ->
            lookupReference r

        E.If { E.condition , E.whenTrue , E.whenFalse } -> do
            conditionType <- checkExpr condition
            whenTrueType <- checkExpr whenTrue
            whenFalseType <- checkExpr whenFalse

            if conditionType /= T.Bool then
                failCheck <| IfConditionMustBeBool condition

            else if whenTrueType /= whenFalseType then
                BothIfExpressionsMustHaveSameType whenTrue whenFalse
                    |> failCheck

            else
                let
                    sharedReturnType = whenTrueType
                in
                return sharedReturnType

        E.LetIn { E.definitions, E.body } -> do
            withDefinitions definitions expr (checkExpr body)

        _ ->
            failCheck <| TODO expr


withDefinitions :: NonEmpty E.Definition -> E.Expr -> Check -> Check
withDefinitions definitions expression innerChecker =
    let
        toReference definition=
            case definition of
                E.SimpleDefinition id expr -> do
                    type_ <- checkExpr expr
                    return (id, type_)
    in do
    references <-
        definitions
            |> traverse toReference
            |> map NonEmpty.toList
    withEnv references expression innerChecker


checkValue :: E.Value -> Check
checkValue v =
    case v of
        E.Bool _ ->
            return T.Bool

        E.Char _ ->
            return T.Char

        E.Float _ ->
            return T.Float

        E.Int _ ->
            return T.Int

        E.String _ ->
            return T.String
