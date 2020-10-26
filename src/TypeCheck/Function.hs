module TypeCheck.Function (checkFunction) where

import qualified Data.List as List
import qualified Data.Tuple as Tuple

import qualified AST.Expression as E
import qualified Type as T
import TypeCheck.Check (Check)
import qualified TypeCheck.Check as Check
import qualified TypeCheck.Expression as Expression


checkFunction :: T.Type -> [E.Identifier] -> E.Expr -> Check T.Type
checkFunction type_ params body = do
    functionType <- returningFunctionType type_ params

    let ps = paramsWithTypes type_ params
    expressionType  <-
        Check.withVariables ps <| Expression.checkExpression body

    if functionType == expressionType then
        return functionType

    else
        Check.FunctionBodyMismatch functionType expressionType
            |> Check.fail


-- TODO Shadowing
-- TODO Merge with returningFunctionType
paramsWithTypes :: T.Type -> [E.Identifier] -> [( E.Identifier, T.Type )]
paramsWithTypes functionType functionParams =
    let
        addParam paramId type_ params =
            (paramId, type_) : params
    in
    functionParams
        |> List.foldl
            (\(currentType, params) paramId -> do
                case currentType of
                    T.Function (T.FunctionType a b) ->
                        addParam paramId a params
                            |> (\newEnv -> (b, newEnv))

                    _ ->
                        (currentType, [])
            )
            (functionType, [])
        |> Tuple.snd


returningFunctionType :: T.Type -> [E.Identifier] -> Check T.Type
returningFunctionType type_ params =
    List.foldl
        (\result _ -> do
            currentType <- result

            case currentType of
                T.Function (T.FunctionType _ returnValue) ->
                    return returnValue

                _ ->
                    Check.fail <| Check.TooManyParam type_ params
        )
        (return type_)
        params

