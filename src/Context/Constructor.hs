module Context.Constructor
    ( Declaration(..)
    , context
    ) where

import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty

import AST.Identifier (TypeId, TypeVariableId)
import qualified AST.Identifier as Identifier
import qualified AST.Module as M
import AST.TypeAnnotation (TypeAnnotation)
import qualified AST.TypeAnnotation as TA
import Parser.Model.Quote (Quote(..))
import Utils.OrderedSet (OrderedSet)
import qualified Utils.OrderedSet as OrderedSet


data Declaration =
    Declaration
        { name :: String
        , quote :: Quote
        , annotation :: TypeAnnotation
        }
        deriving (Eq, Show)


context :: [M.TopLevel] -> [Declaration]
context =
    bind
        (\topLevel ->
            case topLevel of
                M.SumType { M.typeName, M.typeVariables, M.dataChoices } -> do
                    sumType typeName typeVariables dataChoices

                M.Record {} -> do
                    [] -- TODO

                M.Function {} ->
                    []
        )


sumType
    :: TypeId
    -> OrderedSet TypeVariableId
    -> NonEmpty M.DataChoice
    -> [Declaration]
sumType typeId typeVariableIds dataChoices =
    let
        finalAnnotation =
            typeVariableIds
                |> OrderedSet.toList
                |> map TA.Variable
                |> TA.Custom typeId
    in
    dataChoices
        |> NonEmpty.toList
        |> map
            (\M.DataChoice { tag, args } ->
                let
                    name =
                        Identifier.formatConstructorId tag

                    typeAnnotation =
                        constructorAnnotation args finalAnnotation

                    quote =
                        Quote "aFilePath" 1 1 1 1
                in
                Declaration name quote typeAnnotation
            )


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
