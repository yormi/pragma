module Type.Constraint.Gatherer.LetIn
    ( gatherer
    ) where

import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified AST.Expression as E
import qualified Type.Model as T
import Type.Constraint.Gatherer.Model (Gatherer)
import qualified Type.Constraint.Gatherer.Model as Gatherer
import qualified Type.Constraint.Model as Constraint
import Type.Constraint.Reference (Reference)
import qualified Type.Constraint.Reference as Reference
import qualified Utils.List as List
import qualified Utils.Maybe as Maybe


type ExpressionGatherer =
    E.Expression -> Gatherer T.Type


type DefinitionWithPlaceholder =
    (E.Definition, T.TypePlaceholder)


gatherer
    :: ExpressionGatherer
    -> NonEmpty E.Definition
    -> E.Expression
    -> Gatherer T.Type
gatherer expressionGatherer definitions body = do
    referencesInScope <- Gatherer.referencesInScope
    let graph = defineGraph referencesInScope definitions

    orderedDefinitions <-
        case main graph of
            Just order ->
                order
                    |> traverse (findDefinition definitions)
                    |> map return
                    |> Maybe.withDefault
                        (Gatherer.ShouldNotHappen "Let..In Gatherer - The definition should exist"
                            |> Gatherer.fail
                        )

            Nothing ->
                Gatherer.fail Gatherer.CyclicDependenciesInLetDefinitions


    placeholders <- definitionPlaceholders orderedDefinitions
    let references = newReferences placeholders


    Gatherer.withData references <| do
        addDefinitionConstraints expressionGatherer placeholders
        expressionGatherer body


findDefinition :: NonEmpty E.Definition -> Reference -> Maybe E.Definition
findDefinition definitions reference =
    List.find
        (\d ->
            case d of
                E.SimpleDefinition dataId _ ->
                    Reference.fromDataId dataId == reference
        )
        definitions


--- CYCLE ---


type Dependencies =
    Set Reference


-- Topological Sort
-- Kahn's Algorithm
-- https://en.wikipedia.org/wiki/Topological_sorting#Kahn's_algorithm
main :: Map Reference Dependencies -> Maybe [Reference]
main graph =
    let
        withoutDependencies =
            graph
                |> Map.filter (not << hasDependencies)
                |> Map.keys
                |> Set.fromList

        withDependencies =
            Map.filter hasDependencies graph

        emptyOrder =
            []
    in
    sort emptyOrder withoutDependencies withDependencies


sort
    :: [Reference]
    -> Set Reference
    -> Map Reference Dependencies
    -> Maybe [Reference]
sort order withoutDeps withDeps =
    case Set.toList withoutDeps of
        currentDefinition : rest ->
            let
                newOrder =
                    order ++ [ currentDefinition ]

                withoutDependencyOnCurrentDefinition =
                    map
                        (removeDependencyOnDefinition currentDefinition)
                        withDeps

                newWithDeps =
                    withoutDependencyOnCurrentDefinition
                        |> Map.filter hasDependencies

                newWithoutDeps =
                    (withoutDependencyOnCurrentDefinition
                        |> Map.filter (not << hasDependencies)
                        |> Map.keys
                    )
                        ++ rest
                        |> Set.fromList

            in
            sort newOrder newWithoutDeps newWithDeps

        [] ->
            if List.isEmpty <| Map.toList withDeps then
                Just order

            else
                Nothing


removeDependencyOnDefinition :: Reference -> Dependencies -> Dependencies
removeDependencyOnDefinition =
    Set.delete


defineGraph
    :: Set Reference -> NonEmpty E.Definition -> Map Reference Dependencies
defineGraph referencesInScope =
    NonEmpty.toList
        >> map
            (\d ->
                case d of
                    E.SimpleDefinition dataId expression ->
                        referencesInExpression expression
                            |> Set.filter
                                (\r -> not <| Set.member r referencesInScope)
                            |> (\rs -> (Reference.fromDataId dataId, rs))
            )
        >> Map.fromList


hasDependencies :: Dependencies -> Bool
hasDependencies =
    not << List.isEmpty << Set.toList


referencesInExpression :: E.Expression -> Set Reference
referencesInExpression expression =
    case expression of
        E.Reference r ->
            Reference.fromReferenceId r
                |> Set.singleton

        _ ->
            Set.empty


------


definitionPlaceholders :: [E.Definition] -> Gatherer [DefinitionWithPlaceholder]
definitionPlaceholders definitions = do
    traverse (const Gatherer.nextPlaceholder) definitions
        |> map (List.zip definitions)


newReferences :: [DefinitionWithPlaceholder] -> Map Reference T.Type
newReferences placeholders =
    let
        toReference (definition, placeholder) =
            case definition of
                E.SimpleDefinition id _ ->
                    let
                        reference =
                            Reference.fromDataId id

                        type_ =
                            T.Placeholder placeholder
                    in
                    (reference, type_)
    in
    placeholders
        |> map toReference
        |> Map.fromList


addDefinitionConstraints
    :: ExpressionGatherer -> [DefinitionWithPlaceholder] -> Gatherer ()
addDefinitionConstraints expressionGatherer definitionsWithPlaceholders =
    let
        addConstraint (definition, placeholder) =
            case definition of
                E.SimpleDefinition id expr -> do
                    (generated, type_) <-
                        expressionGatherer expr
                            |> Gatherer.recordingGeneratedPlaceholder

                    let reference = Reference.fromDataId id
                    Constraint.LetDefinition
                        reference
                        type_
                        placeholder
                        generated
                        |> Gatherer.addConstraint


                    return (reference, T.Placeholder placeholder)
    in
    definitionsWithPlaceholders
        |> traverse addConstraint
        |> void

