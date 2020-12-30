module Type.Constraint.Gatherer.TypeAnnotation
    ( gather
    , toType
    ) where

import Data.Set (Set)
import qualified Data.Set as Set

import AST.TypeAnnotation (TypeAnnotation)
import qualified AST.TypeAnnotation as Annotation
import AST.Identifier (TypeVariableId)
import qualified Type.Model as T
import qualified Utils.List as List


type TypeContext =
    Set TypeVariableId


initialContext :: TypeContext
initialContext =
    Set.empty


gather :: TypeAnnotation -> (TypeContext, T.Type)
gather typeAnnotation =
    let
        context =
            typeVariableContext typeAnnotation

        type_ =
            toType typeAnnotation
    in
    (context, type_)


typeVariableContext :: TypeAnnotation -> TypeContext
typeVariableContext typeAnnotation =
    let
        variableIdentifiers =
            Annotation.extractTypeVariables typeAnnotation
    in
    List.foldl
        (\resultingContext typeVariableId ->
            Set.insert typeVariableId resultingContext
        )
        initialContext
        variableIdentifiers


toType :: TypeAnnotation -> T.Type
toType annotation =
    case annotation of
        Annotation.Bool ->
            T.Bool

        Annotation.Int ->
            T.Int

        Annotation.Float ->
            T.Float

        Annotation.Char ->
            T.Char

        Annotation.String ->
            T.String

        Annotation.Function { arg , returnType } ->
            let
                argType =
                    toType arg

                returning =
                    toType returnType

            in
            T.FunctionType argType returning
                |> T.Function


        Annotation.Custom { typeName, args } ->
            let
                argTypes =
                    map toType args
            in
            T.Custom typeName argTypes


        Annotation.Variable identifier ->
            T.Variable identifier
