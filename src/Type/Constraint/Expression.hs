module Type.Constraint.Expression (gather) where

import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty

import AST.CodeQuote (CodeQuote)
import qualified AST.Expression as E
import qualified Type as T
import Type.Constraint.Gatherer (Gatherer)
import qualified Type.Constraint.Gatherer as Gatherer
import qualified Type.Constraint.Model as Constraint


gather :: E.QuotedExpression -> Gatherer T.Type
gather expression =
    case E.expression expression of
        E.Value v ->
            return <| valueType v


        E.Reference identifier ->
            Gatherer.lookupReference identifier


        E.If { E.condition, E.whenTrue, E.whenFalse } -> do
            conditionType <- gather condition
            whenTrueType <- gather whenTrue
            whenFalseType <- gather whenFalse

            ifType <- Gatherer.freshVariable

            let quote = (E.codeQuote :: E.QuotedExpression -> CodeQuote)

            Constraint.IfThenElse
                { Constraint.codeQuote = E.codeQuote expression
                , Constraint.condition =
                    Constraint.QuotedType (quote condition) conditionType
                , Constraint.whenTrue =
                    Constraint.QuotedType (quote whenTrue) whenTrueType
                , Constraint.whenFalse =
                    Constraint.QuotedType (quote whenFalse) whenFalseType
                , Constraint.returnType = ifType
                }
                |> Gatherer.addConstraint

            return ifType


        E.LetIn { E.definitions, E.body } ->
            let
                toReference definition=
                    case definition of
                        E.SimpleDefinition id expr -> do
                            type_ <- gather expr
                            return (id, type_)
            in do
            references <-
                definitions
                    |> traverse toReference
                    |> map NonEmpty.toList
            Gatherer.withEnv references (gather body)


        --E.CaseOf { E.element, E.cases } ->


        E.Lambda { E.params, E.body } -> do
            let paramList = NonEmpty.toList params
            variables <- traverse (const Gatherer.freshVariable) paramList
            let paramWithTypes = List.zip paramList variables
            bodyType <- Gatherer.withEnv paramWithTypes (gather body)

            variables
                |> List.foldl
                    (\lambdaType paramType ->
                        T.FunctionType paramType lambdaType
                            |> T.Function
                    )
                    bodyType
                |> return


        E.Application { E.functionName, E.args } -> do
            referenceType <- Gatherer.lookupReference functionName
            argsType <- traverse gather args
            returnType <- Gatherer.freshVariable

            Constraint.Application
                { Constraint.codeQuote = E.codeQuote expression
                , Constraint.functionName = functionName
                , Constraint.args = args
                , Constraint.functionReference = referenceType
                , Constraint.argTypes = argsType
                , Constraint.returnType = returnType
                }
                |> Gatherer.addConstraint

            return returnType

        _ ->
            Gatherer.fail <| Gatherer.TODO "Gather Expression"


valueType :: E.Value -> T.Type
valueType value =
    case value of
        E.Bool _ ->
            T.Bool

        E.Char _ ->
            T.Char

        E.Float _ ->
            T.Float

        E.Int _ ->
            T.Int

        E.String _ ->
            T.String
