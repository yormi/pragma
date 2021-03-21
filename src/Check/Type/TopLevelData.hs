module Check.Type.TopLevelData (TopLevelData(..), filter) where

import qualified AST.Expression as Expression
import AST.Identifier (DataId)
import qualified AST.Identifier as Identifier
import AST.Module (TopLevel)
import qualified AST.Module as Module
import AST.TypeAnnotation (TypeAnnotation)
import qualified AST.TypeAnnotation as TA
import qualified Check.Type.Context as Context
import qualified Utils.List as List
import qualified Utils.Maybe as Maybe

import GHC.Err as GHC

data TopLevelData =
    TopLevelData
        { paramTypes :: [ (DataId, TypeAnnotation) ]
        , expectedReturnType :: TypeAnnotation
        , body :: Expression.Expression
        }
        deriving (Eq, Show)


filter :: Context.Context -> [TopLevel] -> [ TopLevelData ]
filter context =
    map (topLevelData context)
        >> Maybe.values


topLevelData :: Context.Context -> TopLevel -> Maybe TopLevelData
topLevelData context topLevel =
    case topLevel of
        Module.Function { functionName, params, body } ->
            context
                |> Context.lookupReference
                    (Identifier.formatDataId functionName)
                |> map
                    (\t ->
                        TopLevelData
                            { paramTypes = paramsType params t
                            , expectedReturnType = returningType params t
                            , body = body
                            }
                      )

        _ ->
            Nothing


paramsType :: [ DataId ] -> TypeAnnotation -> [ ( DataId, TypeAnnotation ) ]
paramsType params signatureType =
    case (params, signatureType) of
        (p : otherParams, TA.Function { arg, returnType }) ->
            (p, arg) : paramsType otherParams returnType

        ([], _) ->
            []

        _ ->
            GHC.error "Check.Type.ReplaceReference - The number of parameters should have been checked before"


returningType :: [DataId] -> TypeAnnotation -> TypeAnnotation
returningType params signatureType =
    List.foldl
        (\remainingType _ ->
            case remainingType of
                TA.Function { returnType } ->
                    returnType

                _ ->
                    GHC.error "Check.Type.ReplaceReference - The number of parameters should have been checked before"
        )
        signatureType
        params
