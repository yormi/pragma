module Type.Constraint.Gatherer.Module (gather) where

import AST.Identifier (DataId)
import qualified AST.Module as M
import qualified Type.Model as T
import qualified Type.Constraint.Gatherer.Expression as Expression
import Type.Constraint.Gatherer.Model (Gatherer)
import qualified Type.Constraint.Gatherer.Model as Gatherer
import qualified Type.Constraint.Gatherer.TypeAnnotation as TypeAnnotation
import qualified Type.Constraint.Model as Constraint
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
            -> do
            let (typeVariables, type_) = TypeAnnotation.gather typeAnnotation
            paramsWithTypes_ <- paramsWithTypes type_ params

            bodyType <-
                Expression.gather body
                    |> Gatherer.withData paramsWithTypes_

            Constraint.Function
                { Constraint.codeQuote = codeQuote
                , Constraint.signatureType = type_
                , Constraint.params = map snd paramsWithTypes_
                , Constraint.body = bodyType
                }
                |> Gatherer.addConstraint

            returnType <- Gatherer.freshVariable
            Constraint.Definition functionName type_ returnType
                |> Gatherer.addConstraint

        M.SumType {} ->
            return ()


paramsWithTypes
    :: T.Type
    -> [DataId]
    -> Gatherer [ (DataId, T.Type) ]
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
