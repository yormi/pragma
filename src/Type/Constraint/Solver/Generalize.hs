module Type.Constraint.Solver.Generalize
    ( fromDefinition
    , fromTypeAnnotation
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


fromTypeAnnotation :: TypeAnnotation -> GenericType
fromTypeAnnotation annotation =
    case annotation of
        TA.Bool ->
            G.Bool

        TA.Int ->
            G.Int

        TA.Float ->
            G.Float

        TA.Char ->
            G.Char

        TA.String ->
            G.String

        TA.Function { arg, returnType } ->
            G.Function (fromTypeAnnotation arg) (fromTypeAnnotation returnType)

        TA.Custom { typeName, args } ->
            G.Custom typeName (map fromTypeAnnotation args)

        TA.Variable id ->
            G.Variable id



fromDefinition :: Set T.TypePlaceholder -> InstancedType -> Solver G.GenericType
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
        |> return



replacePlaceholders
    :: Map T.TypePlaceholder TypeVariableId
    -> InstancedType
    -> G.GenericType
replacePlaceholders variableMapping type_ =
    case type_ of
        Bool ->
            G.Bool

        Int ->
            G.Int

        Float ->
            G.Float

        Char ->
            G.Char

        String ->
            G.String

        Function arg returnType ->
            let
                argAnnotation =
                    replacePlaceholders variableMapping arg
                returnAnnotation =
                    replacePlaceholders variableMapping returnType
            in
            G.Function argAnnotation returnAnnotation

        Custom typeName args -> do
            args
                |> map (replacePlaceholders variableMapping)
                |> G.Custom typeName

        Unbound name placeholder ->
            G.ParentVariable name placeholder

        Placeholder placeholder ->
            let
                replacement =
                    Map.lookup placeholder variableMapping
            in
            case replacement of
                Just v ->
                    G.Variable v

                Nothing ->
                    G.Placeholder placeholder
