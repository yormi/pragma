module Type.Constraint.Expression (gather) where

import qualified Data.List.NonEmpty as NonEmpty

import qualified AST.Expression as E
import qualified Type as T
import Type.Constraint.Gatherer (Gatherer)
import qualified Type.Constraint.Gatherer as Gatherer


gather :: E.Expr -> Gatherer T.Type
gather expression =
    case expression of
        E.Value v ->
            return <| valueType v


        E.Reference identifier ->
            Gatherer.lookupReference identifier
                |> map T.Variable


        E.If { E.condition, E.whenTrue, E.whenFalse } -> do
            conditionType <- gather condition
            whenTrueType <- gather whenTrue
            whenFalseType <- gather whenFalse

            Gatherer.addConstraint T.Bool conditionType
            Gatherer.addConstraint whenTrueType whenFalseType

            let sharedReturnType = whenTrueType
            return sharedReturnType


        E.LetIn { E.definitions, E.body } ->
            let
                toReference definition=
                    case definition of
                        E.SimpleDefinition id expr -> do
                            type_ <- gather expr

                            typeVariable <- Gatherer.freshVariable
                            let freshType = T.Variable typeVariable
                            Gatherer.addConstraint freshType type_

                            return (id, typeVariable)
            in do
            references <-
                definitions
                    |> traverse toReference
                    |> map NonEmpty.toList
            Gatherer.withEnv references (gather body)

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
