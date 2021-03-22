module Check.Type.Context
    ( Context
    , build
    , lookupReference
    ) where

import AST.Identifier (TypeVariableId)
import qualified AST.Identifier as Identifier
import qualified AST.Module as M
import AST.TypeAnnotation (TypeAnnotation)
import qualified AST.TypeAnnotation as TA
import qualified Utils.List as List
import qualified Utils.Map as Map
import Utils.NonEmpty (NonEmpty)
import qualified Utils.NonEmpty as NonEmpty
import Utils.OrderedSet (OrderedSet)
import qualified Utils.OrderedSet as OrderedSet


type Context =
    Map Reference TypeAnnotation


type Reference =
    String


lookupReference :: Reference -> Context -> Maybe TypeAnnotation
lookupReference =
    Map.lookup


-- ACTION


build :: M.Module -> Context
build (M.Module topLevels) =
    topLevels
        |> map dataInTopLevel
        |> Map.unions


dataInTopLevel :: M.TopLevel -> Context
dataInTopLevel topLevel =
    case topLevel of
        M.Function { typeAnnotation, functionName } -> do
            ( Identifier.formatDataId functionName, typeAnnotation )
                |> List.singleton
                |> Map.fromList

        M.SumType { typeName, typeVariables, dataChoices } ->
            sumType typeName typeVariables dataChoices


sumType
    :: Identifier.TypeId
    -> OrderedSet TypeVariableId
    -> NonEmpty M.DataChoice
    -> Context
sumType typeId typeVariableIds dataChoices =
    let
        finalAnnotation =
            typeVariableIds
                |> OrderedSet.toList
                |> map TA.Variable
                |> TA.Custom typeId
    in do
    dataChoices
        |> NonEmpty.toList
        |> map
            (\M.DataChoice { tag, args } ->
                let
                    annotation =
                        constructorAnnotation args finalAnnotation
                in do
                let constructorName = Identifier.formatConstructorId tag
                (constructorName, annotation)
            )
        |> Map.fromList



constructorAnnotation :: [TypeAnnotation] -> TypeAnnotation -> TypeAnnotation
constructorAnnotation args finalAnnotation =
    let
        functionFromArgs argAnnotations =
            case argAnnotations of
                [] ->
                    finalAnnotation

                argAnnotation : rest ->
                    functionFromArgs rest
                        |> TA.Function argAnnotation
    in
    functionFromArgs args
