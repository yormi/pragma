module Check.Type.Cycle
    ( Error(..)
    , defineGraph
    , dependencySort
    ) where

import qualified Data.Set as Set

import qualified Utils.Either as Either
import qualified Utils.List as List
import qualified Utils.Map as Map


type Dependencies a =
    Set a


type Graph a =
    Map a (Dependencies a)


data Error =
    HasACycle


defineGraph :: Ord a => [ (a, Dependencies a) ] -> Graph a
defineGraph =
    Map.fromList


-- Topological Sort
-- Kahn's Algorithm
-- https://en.wikipedia.org/wiki/Topological_sorting#Kahn's_algorithm
dependencySort :: Ord a => Graph a -> Either Error [a]
dependencySort graph =
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
        |> Either.fromMaybe HasACycle


sort :: Ord a => [a] -> Set a -> Graph a -> Maybe [a]
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
            if Map.isEmpty withDeps then
                Just order

            else
                Nothing


removeDependencyOnDefinition :: Ord a => a -> Dependencies a -> Dependencies a
removeDependencyOnDefinition =
    Set.delete


hasDependencies :: Dependencies a -> Bool
hasDependencies =
    not << List.isEmpty << Set.toList
