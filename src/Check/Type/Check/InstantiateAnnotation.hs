module Check.Type.Check.InstantiateAnnotation
    ( instantiate
    ) where

import qualified Data.Map as Map
import qualified Data.Set as Set

import AST.Identifier (TypeVariableId)
import AST.TypeAnnotation (TypeAnnotation)
import qualified AST.TypeAnnotation as TA
import Check.Type.Check.Model.InstancedType (InstancedType(..))
import Check.Type.Check.Model.Checker (Checker)
import qualified Check.Type.Check.Model.Checker as Checker
import qualified Utils.List as List


instantiate :: TypeAnnotation -> Checker InstancedType
instantiate annotation =
    let
        variables =
            TA.extractTypeVariables annotation
                |> Set.toList
    in do
    instances <- traverse (const Checker.nextInstance) variables
    let variableMapping =
            List.zip variables instances
                |> Map.fromList

    replaceVariablesInAnnotation variableMapping annotation



replaceVariablesInAnnotation
    :: Map TypeVariableId InstancedType
    -> TypeAnnotation
    -> Checker InstancedType
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
                    Checker.fail
                        (Checker.ThisIsABug "Variable must all have been assigned an instance just before calling this function")
