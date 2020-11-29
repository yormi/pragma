module Type.Constraint.Module (gather) where

import qualified Data.List as List

import qualified AST.Expression as E
import qualified AST.Module as M
import qualified Type as T
import qualified Type.Constraint.Expression as Expression
import Type.Constraint.Gatherer (Gatherer)
import qualified Type.Constraint.Gatherer as Gatherer
import qualified Type.Constraint.Model as Constraint


gather :: M.TopLevel -> Gatherer ()
gather topLevel =
    case topLevel of
        M.Function { M.type_, M.params, M.body } -> do
            paramTypes <- traverse (const Gatherer.freshVariable) params
            paramsWithTypes_ <- paramsWithTypes type_ params

            bodyType <-
                Gatherer.withEnv paramsWithTypes_ <| Expression.gather body

            Constraint.Function
                { Constraint.functionType = type_
                , Constraint.params = paramTypes
                , Constraint.body = bodyType
                }
                |> Gatherer.addConstraint


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
