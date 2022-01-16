module Check.Type.Check.InstantiateGeneric
    ( instantiate
    ) where

import qualified Data.Map as Map
import qualified Data.Set as Set

import Check.Type.Check.Model.InstancedType (InstanceId, InstancedType(..))
import Check.Type.Check.Model.Checker (Checker)
import qualified Check.Type.Check.Model.Checker as Checker
import qualified Check.Type.Check.Model.GenericType as G
import qualified Utils.List as List


instantiate :: G.GenericType -> Checker InstancedType
instantiate genericType =
    let
        variables =
            extractTypeVariables genericType
                |> Set.toList
    in do
    placeholders <- traverse (const Checker.nextInstance) variables
    let variableMapping =
            List.zip variables placeholders
                |> Map.fromList

    replaceVariables variableMapping genericType


extractTypeVariables :: G.GenericType -> Set G.VariableId
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
    :: Map InstanceId InstancedType
    -> G.GenericType
    -> Checker InstancedType
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


        G.ParentVariable _ instanceId ->
            return <| Instance instanceId


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
                    Checker.fail
                        (Checker.ThisIsABug "Variable must all have been assigned an instance just before calling this function")
