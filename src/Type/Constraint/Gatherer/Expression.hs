module Type.Constraint.Gatherer.Expression (gather) where

import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map

import Parser.Model.Quote (Quote)
import qualified AST.Expression as E
import qualified Type.Model as T
import qualified Type.Constraint.Gatherer.LetIn as LetIn
import Type.Constraint.Gatherer.Model (Gatherer)
import qualified Type.Constraint.Gatherer.Model as Gatherer
import qualified Type.Constraint.Model as Constraint
import qualified Type.Constraint.Reference as Reference


gather :: E.Expression -> Gatherer T.Type
gather expression =
    case expression of
        E.Value v ->
            return <| valueType v


        E.Reference identifier ->
            Gatherer.lookupReference identifier


        E.If { E.condition, E.whenTrue, E.whenFalse } -> do
            conditionType <- gather condition
            whenTrueType <- gather whenTrue
            whenFalseType <- gather whenFalse

            ifType <- Gatherer.nextPlaceholder

            let quote = (E.quote :: E.Expression -> Quote)

            Constraint.IfThenElse
                { Constraint.quote = E.quote expression
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
            LetIn.gatherer gather definitions body


        --E.CaseOf { E.element, E.cases } ->


        E.Lambda { E.params, E.body } -> do
            let paramList =
                    NonEmpty.toList params
                        |> map Reference.fromDataId
            variables <-
                traverse (const Gatherer.nextPlaceholder) paramList
                    |> map (map T.Placeholder)
            let paramWithTypes =
                    List.zip paramList variables
                        |> Map.fromList
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
                { Constraint.quote = E.quote expression
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

        E.Char _ _ ->
            T.Char

        E.Float _ _ ->
            T.Float

        E.Int _ _ ->
            T.Int

        E.String _ _ ->
            T.String
