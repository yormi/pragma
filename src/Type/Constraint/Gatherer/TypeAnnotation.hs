module Type.Constraint.Gatherer.TypeAnnotation
    (gather
    ) where

import Data.Map (Map)
import qualified Data.Map as Map

import AST.TypeAnnotation (TypeAnnotation)
import qualified AST.TypeAnnotation as Annotation
import AST.Identifier (TypeVariableId)
import qualified Type.Model as T
import Type.Constraint.Gatherer.Model (Gatherer)
import qualified Type.Constraint.Gatherer.Model as Gatherer
import qualified Utils.List as List


type TypeContext =
    Map TypeVariableId T.TypeVariable


initialContext :: TypeContext
initialContext =
    Map.empty


gather :: TypeAnnotation -> Gatherer T.Type
gather typeAnnotation = do
    scope <- typeVariableContext typeAnnotation
    typeAnnotationToType scope typeAnnotation


typeVariableContext :: TypeAnnotation -> Gatherer TypeContext
typeVariableContext typeAnnotation =
    let
        variableIdentifiers =
            Annotation.extractTypeVariables typeAnnotation
    in do
    freshTypes <- traverse (const Gatherer.freshVariable) variableIdentifiers
    let variablesWithType = List.zip variableIdentifiers freshTypes

    List.foldl
        (\resultingScope (name, typeVariable) ->
            Map.insert name typeVariable resultingScope
        )
        initialContext
        variablesWithType
        |> return


typeAnnotationToType :: TypeContext -> TypeAnnotation -> Gatherer T.Type
typeAnnotationToType scope annotation =
    case annotation of
        Annotation.Bool ->
            return T.Bool

        Annotation.Int ->
            return T.Int

        Annotation.Float ->
            return T.Float

        Annotation.Char ->
            return T.Char

        Annotation.String ->
            return T.String

        Annotation.Function { arg , returnType } -> do
            argType <- typeAnnotationToType scope arg
            returning <- typeAnnotationToType scope returnType

            T.FunctionType argType returning
                |> T.Function
                |> return


        Annotation.Custom identifier ->
            return <| T.Custom identifier


        Annotation.Variable identifier ->
            let
                variable =
                    Map.lookup identifier scope
            in
            case variable of
                Just v ->
                    return <| T.Variable v

                Nothing ->
                    Gatherer.ShouldNotHappen "The type variable should be in the type scope. It is not"
                        |> Gatherer.fail
