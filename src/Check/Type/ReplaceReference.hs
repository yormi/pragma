module Check.Type.ReplaceReference
    ( TopLevelData(..)
    , Expression
    , replaceReference
    ) where

import Control.Monad.Reader (Reader)
import qualified Control.Monad.Reader as Reader
import qualified Control.Monad.Trans.State as State
import qualified Data.Map as Map
import qualified GHC.Err as GHC

import qualified AST.Expression as E
import AST.Identifier (DataId)
import qualified AST.Identifier as Identifier
import AST.Module (TopLevel)
import qualified AST.Module as M
import Check.Type.Context (Context)
import Check.Type.Model (Type)
import qualified Check.Type.Model as T
import qualified Utils.List as List
import Utils.NonEmpty (NonEmpty)
import qualified Utils.Tuple as Tuple


-- Function Params
-- TopLevel Definition
-- Let In
-- SUBDIVIDE IN 2 ?


data TopLevelData =
    TopLevelData
        { expectedReturnType :: Type
        , body :: Expression
        }
        deriving (Eq, Show)


data Expression
    = Value
    | Placeholder ReferencePlaceholder
    | ContextReference Type
    | LetIn
        { definitions :: NonEmpty Definition
        , body :: Expression
        }
        deriving (Eq, Show)


data Definition =
    Definition
        { placeholder :: ReferencePlaceholder
        , body :: Expression
        }
        deriving (Eq, Show)


newtype ReferencePlaceholder =
    ReferencePlaceholder Int
        deriving (Eq, Show)


type Replacer a =
    State.StateT NextPlaceholder (Reader Context) a


type NextPlaceholder =
    ReferencePlaceholder


nextPlaceholder :: Replacer ReferencePlaceholder
nextPlaceholder = do
    next@(ReferencePlaceholder n) <- State.get
    State.put <| ReferencePlaceholder (n + 1)
    return next


withContextAddition :: Context -> Replacer a -> Replacer a
withContextAddition context =
    Reader.local (Map.union context)


-- ACTION


replaceReference :: Context -> TopLevel -> Maybe TopLevelData
replaceReference context topLevel =
    let
        initialState =
            ReferencePlaceholder 0
    in
    replacer topLevel
        |> flip State.evalStateT initialState
        |> flip Reader.runReader context


replacer :: TopLevel -> Replacer (Maybe TopLevelData)
replacer topLevel =
    case topLevel of
        M.Function { functionName, params, body } ->
            replaceInTopLevelData functionName params body
                |> map Just
        _ ->
            return Nothing


replaceInTopLevelData ::
    DataId -> [DataId] -> E.Expression -> Replacer TopLevelData
replaceInTopLevelData functionName params body = do
    currentContext <- lift Reader.ask
    let signatureType =
            Map.lookup (Identifier.formatDataId functionName)
                currentContext

    case signatureType of
        Just t ->
            let
                returnType =
                    returningType params t

                paramContext =
                    paramsType params t
                        |> map (Tuple.mapFirst Identifier.formatDataId)
                        |> Map.fromList
            in do
            replacedBody <-
                replaceInExpression body
                    |> withContextAddition paramContext

            TopLevelData
                { expectedReturnType = returnType
                , body = replacedBody
                }
                |> return

        Nothing ->
            GHC.error "Check.Type.ReplaceReference - Toplevel data type not found in context"



replaceInExpression :: E.Expression -> Replacer Expression
replaceInExpression expression =
    case expression of
        E.Value _ ->
            return Value

        E.Reference referenceId -> do
            context <- Reader.ask
            let referenceType =
                    Map.lookup (Identifier.formatReferenceId referenceId)
                        context
            case referenceType of
                Just t ->
                    return <| ContextReference t

                Nothing ->
                    map Placeholder nextPlaceholder

        E.LetIn { definitions, body } -> do
            replacedDefinitions <-
                traverse
                    (\(E.SimpleDefinition _ d) -> do
                        p <- nextPlaceholder
                        replacedBody <- replaceInExpression d
                        return <| Definition p replacedBody
                    )
                    definitions
            replacedBody <- replaceInExpression body
            LetIn
                { definitions = replacedDefinitions
                , body = replacedBody
                }
                |> return


paramsType :: [DataId] -> Type -> [ (DataId, Type) ]
paramsType params signatureType =
    case (params, signatureType) of
        (p : otherParams, T.Function { arg, returning }) ->
            (p, arg) : paramsType otherParams returning

        ([], _) ->
            []

        _ ->
            GHC.error "Check.Type.ReplaceReference - The number of parameters should have been checked before"


returningType :: [DataId] -> Type -> Type
returningType params signatureType =
    List.foldl
        (\remainingType _ ->
            case remainingType of
                T.Function { returning } ->
                    returning

                _ ->
                    GHC.error "Check.Type.ReplaceReference - The number of parameters should have been checked before"
        )
        signatureType
        params
