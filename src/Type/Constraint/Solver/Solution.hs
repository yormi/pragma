module Type.Constraint.Solver.Solution
    ( mostPrecised
    , instantiate
    ) where

import qualified Data.Map as Map
import qualified Data.Set as Set

import AST.TypeAnnotation (TypeAnnotation)
import qualified AST.TypeAnnotation as TA
import AST.Identifier (TypeVariableId)
import Type.Constraint.Solver.Model (Solver)
import qualified Type.Constraint.Solver.Model as Solver
import qualified Type.Model as T
import qualified Utils.List as List


mostPrecised :: T.Type -> Solver T.Type
mostPrecised type_ =
    case type_ of
        T.Placeholder p -> do
            solution <- Solver.deducedSoFar
            let morePrecise = Map.lookup p solution

            case morePrecise of
                Just (Solver.ReferenceType _ typeAnnotation) -> do
                    instantiate typeAnnotation

                Just (Solver.InstanceType instancedType) ->
                    mostPrecised instancedType

                _ ->
                    return type_


        T.Custom typeId args -> do
            precisedArgs <-
                traverse mostPrecised args
            T.Custom typeId precisedArgs
                |> return


        T.Function (T.FunctionType arg returnType) -> do
            precisedArg <- mostPrecised arg
            precisedReturnType <- mostPrecised returnType
            T.FunctionType precisedArg precisedReturnType
                |> T.Function
                |> return


        _ ->
            return type_


instantiate :: TypeAnnotation -> Solver T.Type
instantiate typeAnnotation =
    let
        variables =
            TA.extractTypeVariables typeAnnotation
                |> Set.toList
    in do
    placeholders <- traverse (const Solver.nextPlaceholder) variables
    let variableMapping =
            List.zip variables placeholders
                |> Map.fromList

    toType variableMapping typeAnnotation



toType :: Map TypeVariableId T.Type -> TypeAnnotation -> Solver T.Type
toType variableMapping annotation =
    case annotation of
        TA.Bool ->
            return T.Bool

        TA.Int ->
            return T.Int

        TA.Float ->
            return T.Float

        TA.Char ->
            return T.Char

        TA.String ->
            return T.String

        TA.Function { arg , returnType } -> do
            argType <- toType variableMapping arg
            returning <- toType variableMapping returnType

            T.FunctionType argType returning
                |> T.Function
                |> return


        TA.Custom { typeName, args } -> do
            argTypes <- traverse (toType variableMapping) args
            return <| T.Custom typeName argTypes


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
                        (Solver.ShouldNotHappen "Variable must all have been assigned a placeholder just before")
