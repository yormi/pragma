module Type.Constraint.Gatherer.Module (gather) where

import qualified Control.Monad as Monad
import qualified Data.Map as Map
import qualified Data.Set as Set

import AST.Identifier (TypeVariableId)
import qualified AST.Module as M
import AST.TypeAnnotation (TypeAnnotation)
import qualified AST.TypeAnnotation as TA
import qualified Type.Model as T
import qualified Type.Constraint.Gatherer.Expression as Expression
import Type.Constraint.Gatherer.Model (Gatherer)
import qualified Type.Constraint.Gatherer.Model as Gatherer
import qualified Type.Constraint.Model as Constraint
import Type.Constraint.Reference (Reference)
import qualified Type.Constraint.Reference as Reference
import qualified Utils.List as List
import qualified Utils.OrderedSet as OrderedSet


gather :: M.TopLevel -> Gatherer ()
gather topLevel =
    case topLevel of
        M.Function
            { M.quote
            , M.typeAnnotation
            , M.params
            , M.body
            }
            ->
            let
                paramReferences =
                    map Reference.fromDataId params
            in do
            paramTypes <- paramsWithTypes typeAnnotation paramReferences
            bodyType <-
                Expression.gather body
                    |> Gatherer.withData (Map.fromList paramTypes)

            Constraint.Function
                { Constraint.quote = quote
                , Constraint.signatureType = typeAnnotation
                , Constraint.params = map snd paramTypes
                , Constraint.body = bodyType
                }
                |> Gatherer.addConstraint


        M.SumType {} ->
            return  ()


--         M.Record {} ->
--             return  ()


paramsWithTypes
    :: TypeAnnotation -> [Reference] -> Gatherer [(Reference, T.Type)]
paramsWithTypes annotation params = do
    mapping <- variableMapping annotation
    signatureType <- toType mapping annotation
    deduceParamTypes signatureType params


variableMapping
    :: TypeAnnotation -> Gatherer (Map TypeVariableId T.TypePlaceholder)
variableMapping annotation =
    let
        variables =
            TA.extractTypeVariables annotation
                |> Set.toList
    in do
    unboundTypes <- traverse (const Gatherer.nextPlaceholder) variables
    unboundTypes
        |> List.zip variables
        |> Map.fromList
        |> return


toType
    :: Map TypeVariableId T.TypePlaceholder -> TypeAnnotation -> Gatherer T.Type
toType mapping annotation =
    case annotation of
        TA.Bool ->
            return <| T.Bool

        TA.Int ->
            return <| T.Int

        TA.Float ->
            return <| T.Float

        TA.Char ->
            return <| T.Char

        TA.String ->
            return <| T.String

        TA.Function { arg, returnType } -> do
            argType <- toType mapping arg
            returningType <- toType mapping returnType
            T.FunctionType argType returningType
                |> T.Function
                |> return

        TA.Custom { typeName, args } -> do
            argTypes <- traverse (toType mapping) args
            T.Custom typeName (OrderedSet.fromList argTypes)
                |> return

        TA.Variable variableId ->
            case Map.lookup variableId mapping of
                Just placeholder ->
                    return <| T.Unbound variableId placeholder

                Nothing ->
                    Gatherer.ShouldNotHappen "Module Gatherer - All the variable must have a mapping to a placeholder"
                        |> Gatherer.fail


deduceParamTypes :: T.Type -> [Reference] -> Gatherer [ (Reference, T.Type) ]
deduceParamTypes signatureType params =
    params
        |> Monad.foldM
            (\(remainingType_, paramPairs) param -> do
                case remainingType_ of
                    T.Function (T.FunctionType paramType returningType) ->
                        let
                            paramWithTypesResult =
                                 paramPairs ++ [ (param, paramType) ]
                        in
                        return (returningType, paramWithTypesResult)

                    _ ->
                        Gatherer.TooManyParameters signatureType params
                            |> Gatherer.fail
            )
            (signatureType, [])
        |> map snd


