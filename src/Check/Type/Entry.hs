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
import qualified Check.Type.Context as Context
import qualified Check.Type.DeduceType as Deduce
import qualified Check.Type.Futurize as Futurize
import qualified Check.Type.ReplaceTopLevel as ReplaceTopLevel
import qualified Check.Type.Model as T
import qualified Utils.Either as Either
import qualified Utils.List as List
import qualified Utils.Maybe as Maybe

import GHC.Err as GHC


data Error
    = FuturizeError Futurize.Error
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
        |> map (topLevelData moduleContext)
        |> Maybe.values
        |> map
            (\TopLevelData { paramTypes, body } -> do
                deduced <-
                    ReplaceTopLevel.replace moduleContext paramTypes body
                        |> Futurize.futurize
                        |> Either.mapLeft FuturizeError
                        |> map Arrange.arrange
                        |> map Deduce.deduceType

                case Monad.mapM Check.check deduced of
                    Just e ->
                        Left <| TypeCheckErrors e

                    Nothing ->
                        Right ()
            )
        |> Either.lefts


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
