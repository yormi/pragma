module Check.Type.Futurize
    ( Definition(..)
    , Error(..)
    , Expression(..)
    , Placeholder(..)
    , futurize
    ) where

import Control.Monad.Reader (Reader)
import qualified Control.Monad.Reader as Reader
import qualified Control.Monad.Trans.Except as Except
import qualified Control.Monad.Trans.State as State
import qualified Data.Map as Map
import qualified GHC.Err as GHC

import AST.TypeAnnotation (TypeAnnotation)
import qualified Check.Type.ReplaceTopLevel as R
import AST.Identifier (DataId(..), ReferenceId(..))
import qualified Check.Type.Model.PrimitiveType as Primitive
import Parser.Model.Quote (Quote)
import Utils.NonEmpty (NonEmpty)
import qualified Utils.NonEmpty as NonEmpty


newtype Error =
    ReferenceNotInScope Quote
        deriving (Eq, Show)


data Expression
    = Primitive Quote Primitive.Type
    | ContextReference TypeAnnotation
    | Future Placeholder
    | LetIn
        { definitions :: NonEmpty Definition
        , body :: Expression
        }
        deriving (Eq, Show)


data Definition =
    Definition
        { name :: String
        , placeholder :: Placeholder
        , body :: Expression
        }
        deriving (Eq, Show)


newtype Placeholder =
    Placeholder Int
        deriving (Eq, Ord, Show)


type Replacer a =
    Except.ExceptT Error
        (State.StateT NextPlaceholder
            (Reader Context)
        ) a


type Context =
    Map String Placeholder


type NextPlaceholder =
    Placeholder


nextPlaceholder :: Replacer Placeholder
nextPlaceholder = do
    next@(Placeholder n) <- lift <| State.get
    lift <| State.put <| Placeholder (n + 1)
    return next


lookupReference :: String -> Replacer (Maybe Placeholder)
lookupReference id = do
    context <- Reader.ask
    return <| Map.lookup id context


withContextAddition :: Context -> Replacer a -> Replacer a
withContextAddition context =
    Reader.local (Map.union context)


fail :: Error -> Replacer a
fail =
    Except.throwE



-- ACTION


futurize :: R.Expression -> Either Error Expression
futurize expression =
    let
        initialState =
            Placeholder 0

        initialContext =
            Map.empty
    in
    replaceInExpression expression
        |> Except.runExceptT
        |> flip State.evalStateT initialState
        |> flip Reader.runReader initialContext


replaceInExpression :: R.Expression -> Replacer Expression
replaceInExpression expression =
    case expression of
        R.Primitive quote primitiveType ->
            Primitive quote primitiveType
                |> return

        R.Reference (ReferenceId quote name) -> do
            referencePlaceholder <- lookupReference name
            case referencePlaceholder of
                Just p ->
                    return <| Future p

                Nothing ->
                    fail <| ReferenceNotInScope quote

        R.LetIn { definitions, body } ->
            letIn definitions body


letIn :: NonEmpty R.Definition -> R.Expression -> Replacer Expression
letIn definitions body = do
    definitionContext <-
        definitions
            |> traverse
                (\(R.SimpleDefinition (DataId _ name) _) -> do
                    p <- nextPlaceholder
                    return ( name, p )
                )
            |> map NonEmpty.toList
            |> map Map.fromList

    replacedDefinitions <-
        traverse
            (\(R.SimpleDefinition (DataId _ name) d) -> do
                placeholder <- lookupReference name
                case placeholder of
                    Just p -> do
                        replacedBody <- replaceInExpression d
                        return <| Definition name p replacedBody

                    Nothing ->
                        GHC.error "The reference must be defined just above"
            )
            definitions
            |> withContextAddition definitionContext

    replacedBody <-
        replaceInExpression body
            |> withContextAddition definitionContext

    LetIn
        { definitions = replacedDefinitions
        , body = replacedBody
        }
        |> return
