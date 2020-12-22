module Type.Constraint.Gatherer.Module (gather, signatureType) where

import qualified AST.Expression as E
import qualified AST.Module as M
import AST.TypeAnnotation (TypeAnnotation)
import qualified AST.TypeAnnotation as Annotation
import qualified Type as T
import qualified Type.Constraint.Gatherer.Expression as Expression
import Type.Constraint.Gatherer.Model (Gatherer)
import qualified Type.Constraint.Gatherer.Model as Gatherer
import Type.Constraint.Gatherer.TypeScope (TypeScope)
import qualified Type.Constraint.Gatherer.TypeScope as TypeScope
import qualified Type.Constraint.Model as Constraint
import qualified Utils.List as List


gather :: M.TopLevel -> Gatherer ()
gather topLevel =
    case topLevel of
        M.Function { M.codeQuote, M.typeAnnotation, M.params, M.body } -> do
            type_ <- signatureType typeAnnotation
            paramsWithTypes_ <- paramsWithTypes type_ params

            bodyType <-
                Expression.gather body
                    |> Gatherer.withDataReferences paramsWithTypes_

            Constraint.Function
                { Constraint.codeQuote = codeQuote
                , Constraint.signatureType = type_
                , Constraint.params = map snd paramsWithTypes_
                , Constraint.body = bodyType
                }
                |> Gatherer.addConstraint

            returnType <- Gatherer.freshVariable
            Constraint.Generalized type_ returnType
                |> Gatherer.addConstraint


signatureType :: TypeAnnotation -> Gatherer T.Type
signatureType typeAnnotation = do
    scope <- typeVariableScope typeAnnotation
    typeAnnotationToType scope typeAnnotation


typeVariableScope :: TypeAnnotation -> Gatherer TypeScope
typeVariableScope typeAnnotation =
    let
        variableIdentifiers =
            Annotation.extractTypeVariables typeAnnotation
    in do
    freshTypes <- traverse (const Gatherer.freshVariable) variableIdentifiers
    let variablesWithType = List.zip variableIdentifiers freshTypes

    List.foldl
        (\resultingScope (name, typeVariable) ->
            TypeScope.extend name typeVariable resultingScope
        )
        TypeScope.initial
        variablesWithType
        |> return


typeAnnotationToType :: TypeScope -> TypeAnnotation -> Gatherer T.Type
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


        Annotation.Variable identifier ->
            let
                variable =
                    TypeScope.lookup identifier scope
            in
            case variable of
                Just v ->
                    return <| T.Variable v

                Nothing ->
                    Gatherer.ShouldNotHappen "The type variable hould be in the type scope. It is not"
                        |> Gatherer.fail


paramsWithTypes
    :: T.Type
    -> [E.Identifier]
    -> Gatherer [ (E.Identifier, T.Type) ]
paramsWithTypes functionType params =
    List.foldl
        (\result param -> do
            (remainingType_, paramPairs) <- result
            case remainingType_ of
                T.Function (T.FunctionType paramType returningType) ->
                    let
                        paramWithTypesResult =
                            (param, paramType) : paramPairs
                    in
                    return (returningType, paramWithTypesResult)

                _ ->
                    Gatherer.TooManyParameters functionType params
                        |> Gatherer.fail
        )
        (return (functionType, []))
        params
        |> map snd
        |> map List.reverse
