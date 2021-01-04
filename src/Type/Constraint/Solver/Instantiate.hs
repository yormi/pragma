module Type.Constraint.Solver.Instantiate
    ( fromGenericType
    , fromTypeAnnotation
    ) where

import qualified Data.Map as Map
import qualified Data.Set as Set

import AST.Identifier (TypeVariableId)
import AST.TypeAnnotation (TypeAnnotation)
import qualified AST.TypeAnnotation as TA
import Type.Constraint.Solver.Model.Solver (InstancedType(..), Solver)
import qualified Type.Constraint.Solver.Model.Generic as G
import qualified Type.Constraint.Solver.Model.Solver as Solver
import qualified Utils.List as List


fromGenericType :: G.GenericType -> Solver InstancedType
fromGenericType genericType =
    let
        variables =
            extractTypeVariables genericType
                |> Set.toList
    in do
    placeholders <- traverse (const Solver.nextPlaceholder) variables
    let variableMapping =
            List.zip variables placeholders
                |> Map.fromList

    replaceVariables variableMapping genericType


extractTypeVariables :: G.GenericType -> Set TypeVariableId
extractTypeVariables genericType =
    case genericType of
        G.Variable identifier ->
            Set.singleton identifier

        G.Function { arg , returnType } ->
            [ extractTypeVariables arg
            , extractTypeVariables returnType
            ]
                |> Set.unions

        G.Custom { args } ->
            map extractTypeVariables args
                |> Set.unions

        _ ->
            Set.empty


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


fromTypeAnnotation :: TypeAnnotation -> Solver InstancedType
fromTypeAnnotation annotation =
    let
        variables =
            TA.extractTypeVariables annotation
                |> Set.toList
    in do
    placeholders <- traverse (const Solver.nextPlaceholder) variables
    let variableMapping =
            List.zip variables placeholders
                |> Map.fromList

    replaceVariablesInAnnotation variableMapping annotation



replaceVariablesInAnnotation
    :: Map TypeVariableId InstancedType
    -> TypeAnnotation
    -> Solver InstancedType
replaceVariablesInAnnotation variableMapping annotation =
    case annotation of
        TA.Bool ->
            return Bool

        TA.Int ->
            return Int

        TA.Float ->
            return Float

        TA.Char ->
            return Char

        TA.String ->
            return String

        TA.Function { arg , returnType } -> do
            argType <- replaceVariablesInAnnotation variableMapping arg
            returning <- replaceVariablesInAnnotation variableMapping returnType

            Function argType returning
                |> return


        TA.Custom { typeName, args } -> do
            argTypes <-
                traverse (replaceVariablesInAnnotation variableMapping) args
            return <| Custom typeName argTypes


        TA.Variable identifier ->
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
