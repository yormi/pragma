module Type.Constraint.Gatherer.Module (gather) where

import qualified Control.Monad as Monad
import qualified Data.Map as Map
import qualified Data.Set as Set

import AST.Identifier (TypeVariableId)
import qualified AST.Module as M
import qualified Type.Model as T
import qualified Type.Constraint.Gatherer.Expression as Expression
import Type.Constraint.Gatherer.Model (Gatherer)
import qualified Type.Constraint.Gatherer.Model as Gatherer
import qualified Type.Constraint.Gatherer.TypeAnnotation as TypeAnnotation
import qualified Type.Constraint.Model as Constraint
import Type.Constraint.Reference (Reference)
import qualified Type.Constraint.Reference as Reference
import qualified Utils.List as List


gather :: M.TopLevel -> Gatherer ()
gather topLevel =
    case topLevel of
        M.Function
            { M.codeQuote
            , M.functionName
            , M.typeAnnotation
            , M.params
            , M.body
            }
            ->
            let
                signatureType =
                    TypeAnnotation.toType typeAnnotation

                reference =
                    Reference.fromDataId functionName

                paramReferences =
                    map Reference.fromDataId params
            in do
            paramsTypes <- paramsWithTypes signatureType paramReferences
            instantiations <-
                paramsTypes
                    |> Map.elems
                    |> Set.fromList
                    |> instancedVariables

            paramsWithInstancedTypes <-
                paramsTypes
                    |> map (T.replaceVariables instantiations)
                    |> return

            bodyType <-
                Expression.gather body
                    |> Gatherer.withData paramsWithInstancedTypes

            Constraint.Function
                { Constraint.codeQuote = codeQuote
                , Constraint.signatureType = signatureType
                , Constraint.instancedVariables = instantiations
                , Constraint.params = Map.elems paramsWithInstancedTypes
                , Constraint.body = bodyType
                }
                |> Gatherer.addConstraint

            placeholder <- Gatherer.nextPlaceholder
            Constraint.Reference reference signatureType placeholder
                |> Gatherer.addConstraint


        M.SumType {} ->
            return  ()


instancedVariables :: Set T.Type -> Gatherer (Map TypeVariableId T.Type)
instancedVariables types =
    types
        |> Set.toList
        |> map T.variables
        |> Set.unions
        |> Set.toList
        |> traverse
            (\v -> do
                p <- Gatherer.nextPlaceholder
                return (v, T.Placeholder p)
            )
        |> map Map.fromList


paramsWithTypes :: T.Type -> [Reference] -> Gatherer (Map Reference T.Type)
paramsWithTypes signatureType params =
    params
        |> Monad.foldM
            (\(remainingType_, paramPairs) param -> do
                case remainingType_ of
                    T.Function (T.FunctionType paramType returningType) ->
                        let
                            paramWithTypesResult =
                                (param, paramType) : paramPairs
                        in
                        return (returningType, paramWithTypesResult)

                    _ ->
                        Gatherer.TooManyParameters signatureType params
                            |> Gatherer.fail
            )
            (signatureType, [])
        |> map snd
        |> map List.reverse
        |> map Map.fromList
