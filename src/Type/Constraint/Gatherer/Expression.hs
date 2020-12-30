module Type.Constraint.Gatherer.Expression (gather) where

import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty

import AST.CodeQuote (CodeQuote)
import qualified AST.Expression as E
import qualified Type.Model as T
import Type.Constraint.Gatherer.Model (Gatherer)
import qualified Type.Constraint.Gatherer.Model as Gatherer
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

            ifType <- Gatherer.nextPlaceholder

            let quote = (E.codeQuote :: E.QuotedExpression -> CodeQuote)

            Constraint.IfThenElse
                { Constraint.codeQuote = E.codeQuote expression
                , Constraint.condition =
                    Constraint.QuotedType (quote condition) conditionType
                , Constraint.whenTrue =
                    Constraint.QuotedType (quote whenTrue) whenTrueType
                , Constraint.whenFalse =
                    Constraint.QuotedType (quote whenFalse) whenFalseType
                , Constraint.placeholder = ifType
                }
                |> Gatherer.addConstraint

            return <| T.Placeholder ifType


        E.LetIn { E.definitions, E.body } ->
            let
                toReference definition=
                    case definition of
                        E.SimpleDefinition id expr -> do
                            type_ <- gather expr
                            placeholder <- Gatherer.nextPlaceholder

                            Constraint.Definition id type_ placeholder
                                |> Gatherer.addConstraint

                            return (id, T.Placeholder placeholder)
            in do
            references <-
                definitions
                    |> traverse toReference
                    |> map NonEmpty.toList
            Gatherer.withData references (gather body)


        --E.CaseOf { E.element, E.cases } ->


        E.Lambda { E.params, E.body } -> do
            let paramList = NonEmpty.toList params
            variables <-
                traverse (const Gatherer.nextPlaceholder) paramList
                    |> map (map T.Placeholder)
            let paramWithTypes = List.zip paramList variables
            bodyType <- Gatherer.withData paramWithTypes (gather body)

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
            placeholder <- Gatherer.nextPlaceholder

            Constraint.Application
                { Constraint.codeQuote = E.codeQuote expression
                , Constraint.functionName = functionName
                , Constraint.args = args
                , Constraint.functionReference = referenceType
                , Constraint.argTypes = argsType
                , Constraint.placeholder = placeholder
                }
                |> Gatherer.addConstraint

            return <| T.Placeholder placeholder

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
