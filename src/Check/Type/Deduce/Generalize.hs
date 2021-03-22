module Check.Type.Deduce.Generalize
    ( fromDefinition
    ) where

import qualified Data.Map as Map
import qualified Data.Set as Set

import AST.Identifier (TypeVariableId)
import AST.TypeAnnotation (TypeAnnotation)
import qualified AST.TypeAnnotation as TA
import qualified Type.Constraint.Solver.Deduce as Deduce
import Type.Constraint.Solver.Model.Solver (InstancedType(..), Solver)
import qualified Type.Constraint.Solver.Model.Solver as Solver
import Type.Constraint.Solver.Model.Generic (GenericType)
import qualified Type.Constraint.Solver.Model.Generic as G
import qualified Type.Model as T
import qualified Utils.List as List


fromDefinition :: Set T.TypePlaceholder -> InstancedType -> Deducer G.GenericType
fromDefinition expressionPlaceholders type_ =
    let
        placeholderList =
            Set.toList expressionPlaceholders
    in do
    precised <- Deduce.mostPrecised type_
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
