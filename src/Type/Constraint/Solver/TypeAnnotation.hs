module Type.Constraint.Solver.TypeAnnotation
    ( instantiate
    ) where

import qualified Data.Map as Map
import qualified Data.Set as Set

import AST.Identifier (TypeVariableId)
import AST.TypeAnnotation (TypeAnnotation)
import qualified AST.TypeAnnotation as TA
import Type.Constraint.Solver.Model (Solver)
import Type.Constraint.Solver.Model (InstancedType(..))
import qualified Type.Constraint.Solver.Model as Solver
import qualified Utils.List as List


instantiate :: TypeAnnotation -> Solver InstancedType
instantiate annotation =
    let
        variables =
            TA.extractTypeVariables annotation
                |> Set.toList
    in do
    placeholders <- traverse (const Solver.nextInstance) variables
    let variableMapping =
            List.zip variables placeholders
                |> Map.fromList

    replaceVariables variableMapping annotation



replaceVariables
    :: Map TypeVariableId InstancedType
    -> TypeAnnotation
    -> Solver InstancedType
replaceVariables variableMapping annotation =
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
            argType <- replaceVariables variableMapping arg
            returning <- replaceVariables variableMapping returnType

            Function argType returning
                |> return


        TA.Custom { typeName, args } -> do
            argTypes <- traverse (replaceVariables variableMapping) args
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
