module Context.Constructor
    ( Context
    , asMap
    , context
    , lookup
    ) where

import qualified Data.Map as Map
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty

import AST.Identifier (ConstructorId, TypeId, TypeVariableId)
import qualified AST.Module as M
import AST.TypeAnnotation (TypeAnnotation)
import qualified AST.TypeAnnotation as TA
import Parser.Model.Quote (Quote)
import qualified Utils.List as List
import Utils.OrderedSet (OrderedSet)
import qualified Utils.OrderedSet as OrderedSet


newtype Context
    = Context (Map ConstructorId TypeAnnotation)
        deriving (Eq, Show)

data Error
    = ConstructorNameAlreadyExists
        { quote :: Quote
        , typeId :: TypeId
        }
        deriving (Eq, Show)


initialContext :: Context
initialContext =
    Context Map.empty


lookup :: ConstructorId -> Context -> Maybe TypeAnnotation
lookup id (Context c)=
    Map.lookup id c


context :: [M.TopLevel] -> Context
context topLevels =
    List.foldl
        ( \resultingContext topLevel ->
            case topLevel of
                M.SumType { M.typeName, M.typeVariables, M.dataChoices } -> do
                    sumType typeName typeVariables dataChoices resultingContext

--                 M.Record {} -> do
--                     resultingContext -- TODO

                M.Function {} ->
                    resultingContext

        )
        initialContext
        topLevels


sumType
    :: TypeId
    -> OrderedSet TypeVariableId
    -> NonEmpty M.DataChoice
    -> Context
    -> Context
sumType typeId typeVariableIds dataChoices (Context c) =
    let
        finalAnnotation =
            typeVariableIds
                |> OrderedSet.toList
                |> map TA.Variable
                |> TA.Custom typeId
    in
    dataChoices
        |> NonEmpty.toList
        |> List.foldl
            (\resultingContext M.DataChoice { tag, args } ->
                let
                    typeAnnotation =
                        constructorAnnotation args finalAnnotation
                in
                Map.insert tag typeAnnotation resultingContext
            )
            c
        |> Context


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


asMap :: Context -> Map ConstructorId TypeAnnotation
asMap (Context c) =
    c
