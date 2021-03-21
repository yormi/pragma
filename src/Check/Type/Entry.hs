module Check.Type.Entry
    ( typeCheck
    ) where

import qualified Control.Monad as Monad
import qualified Data.Map as Map

import qualified AST.Expression as Expression
import AST.Identifier (DataId)
import qualified AST.Identifier as Identifier
import AST.Module (Module(..), TopLevel)
import qualified AST.Module as Module
import Check.Type.Check (TypeError)
import qualified Check.Type.Arrange as Arrange
import qualified Check.Type.Check as Check
import Check.Type.Constraint (Constraint)
import qualified Check.Type.Constraint as Constraint
import Check.Type.Context (Context)
import qualified Check.Type.Context as Context
import Check.Type.Deduce (Deduced)
import qualified Check.Type.Deduce as Deduce
import qualified Check.Type.Futurize as Futurize
import qualified Check.Type.ReplaceTopLevel as ReplaceTopLevel
import qualified Check.Type.Model as T
import qualified Utils.Either as Either
import qualified Utils.List as List
import qualified Utils.Maybe as Maybe

import GHC.Err as GHC


data Error
    = FuturizeError Futurize.Error
    | ArrangeError Arrange.Error
    | DeduceError Deduce.Error
    | ConstraintError Constraint.Error
    | TypeCheckErrors [TypeError]


data TopLevelData =
    TopLevelData
        { paramTypes :: [ (DataId, T.Type) ]
        , expectedReturnType :: T.Type
        , body :: Expression.Expression
        }
        deriving (Eq, Show)


typeCheck :: Module -> [Error]
typeCheck module_@(Module topLevels) =
    let
        moduleContext =
            Context.build module_
    in
    topLevels
        |> filterTopLevelData moduleContext
        |> map
            (\topLevel -> do
                arranged <- arrange moduleContext topLevel
                deduced <- deduce arranged
                constraints <- buildConstraint deduced arranged
                check constraints
            )
        |> Either.lefts


arrange :: Context -> TopLevelData -> Either Error [ Arrange.Expression ]
arrange moduleContext TopLevelData { paramTypes, body } =
    ReplaceTopLevel.replace moduleContext paramTypes body
        |> Futurize.futurize
        |> Either.mapLeft FuturizeError
        |> bind (Arrange.arrange >> Either.mapLeft ArrangeError)


deduce :: [ Arrange.Expression ] -> Either Error Deduced
deduce =
    Deduce.deduceType >> Either.mapLeft DeduceError


buildConstraint
    :: Deduced -> [ Arrange.Expression ] -> Either Error [ Constraint ]
buildConstraint deduced =
    Constraint.build deduced
        >> Either.mapLeft ConstraintError


check :: [ Constraint ] -> Either Error ()
check deduced =
    Monad.mapM Check.check deduced
        |> map TypeCheckErrors
        |> Either.fromMaybeError



filterTopLevelData :: Context.Context -> [TopLevel] -> [ TopLevelData ]
filterTopLevelData context =
    map (topLevelData context)
        >> Maybe.values


topLevelData :: Context.Context -> TopLevel -> Maybe TopLevelData
topLevelData context topLevel =
    case topLevel of
        Module.Function { functionName, params, body } ->
            let
                signatureType =
                    Map.lookup (Identifier.formatDataId functionName) context
            in
            map
                (\t ->
                    TopLevelData
                        { paramTypes = paramsType params t
                        , expectedReturnType = returningType params t
                        , body = body
                        }
                  )
                signatureType

        _ ->
            Nothing


paramsType :: [DataId] -> T.Type -> [ (DataId, T.Type) ]
paramsType params signatureType =
    case (params, signatureType) of
        (p : otherParams, T.Function { arg, returning }) ->
            (p, arg) : paramsType otherParams returning

        ([], _) ->
            []

        _ ->
            GHC.error "Check.Type.ReplaceReference - The number of parameters should have been checked before"


returningType :: [DataId] -> T.Type -> T.Type
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
