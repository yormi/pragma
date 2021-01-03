module Type.Constraint.Solver.Solution
    ( generalize
    , instantiate
    , mostPrecised
    ) where

import qualified Data.Map as Map
import qualified Data.Set as Set

import AST.Identifier (TypeVariableId)
import Type.Constraint.Solver.Model (InstancedType(..), Solver)
import qualified Type.Constraint.Solver.Generic as G
import qualified Type.Constraint.Solver.Model as Solver
import qualified Type.Model as T
import qualified Utils.List as List


mostPrecised :: InstancedType -> Solver InstancedType
mostPrecised type_ =
    case type_ of
        Placeholder p -> do
            solution <- Solver.deducedSoFar
            let morePrecise = Map.lookup p solution

            case morePrecise of
                Just (Solver.Generic _ genericType) -> do
                    instantiate genericType

                Just (Solver.Instanced instancedType) ->
                    mostPrecised instancedType

                _ ->
                    return type_


        Custom typeId args -> do
            precisedArgs <-
                traverse mostPrecised args
            Custom typeId precisedArgs
                |> return


        Function arg returnType -> do
            precisedArg <- mostPrecised arg
            precisedReturnType <- mostPrecised returnType
            Function precisedArg precisedReturnType
                |> return


        _ ->
            return type_


instantiate :: G.GenericType -> Solver InstancedType
instantiate genericType =
    let
        variables =
            G.extractTypeVariables genericType
                |> Set.toList
    in do
    placeholders <- traverse (const Solver.nextPlaceholder) variables
    let variableMapping =
            List.zip variables placeholders
                |> Map.fromList

    replaceVariables variableMapping genericType
        |> trace (show genericType ++ "\n" ++ show placeholders)


replaceVariables
    :: Map TypeVariableId InstancedType
    -> G.GenericType
    -> Solver InstancedType
replaceVariables variableMapping annotation =
    case annotation of
        G.Bool ->
            return Bool

        G.Int ->
            return Int

        G.Float ->
            return Float

        G.Char ->
            return Char

        G.String ->
            return String

        G.Function { arg , returnType } -> do
            argType <- replaceVariables variableMapping arg
            returning <- replaceVariables variableMapping returnType

            Function argType returning
                |> return


        G.ParentVariable name placeholder ->
            return <| Unbound name placeholder


        G.Placeholder p ->
            return <| Placeholder p


        G.Custom { typeName, args } -> do
            argTypes <- traverse (replaceVariables variableMapping) args
            return <| Custom typeName argTypes


        G.Variable identifier ->
            let
                replacement =
                    Map.lookup identifier variableMapping
            in
            case replacement of
                Just r ->
                    return r

                Nothing ->
                    Solver.fail
                        (Solver.ShouldNotHappen "Variable must all have been assigned an instance just before calling this function")


generalize :: Set T.TypePlaceholder -> InstancedType -> Solver G.GenericType
generalize expressionPlaceholders type_ =
    let
        placeholderList =
            Set.toList expressionPlaceholders
    in do
    precised <- mostPrecised type_
    variables <- traverse (const Solver.nextVariable) placeholderList
    let variableMapping =
            List.zip placeholderList variables
                |> Map.fromList

    replacePlaceholders variableMapping precised


replacePlaceholders
    :: Map T.TypePlaceholder TypeVariableId
    -> InstancedType
    -> Solver G.GenericType
replacePlaceholders variableMapping type_ =
    case type_ of
        Bool ->
            return G.Bool

        Int ->
            return G.Int

        Float ->
            return G.Float

        Char ->
            return G.Char

        String ->
            return G.String

        Function arg returnType -> do
            argAnnotation <- replacePlaceholders variableMapping arg
            returnAnnotation <- replacePlaceholders variableMapping returnType
            G.Function argAnnotation returnAnnotation
                |> return

        Custom typeName args -> do
            argsAnnotation <-
                traverse (replacePlaceholders variableMapping) args
            G.Custom typeName argsAnnotation
                |> return

        Unbound name placeholder ->
            G.ParentVariable name placeholder
                |> return

        Placeholder placeholder ->
            let
                replacement =
                    Map.lookup placeholder variableMapping
            in
            case replacement of
                Just v ->
                    return <| G.Variable v

                Nothing ->
                    return <| G.Placeholder placeholder
