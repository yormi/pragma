module TypeCheck.Expression
    ( checkExpression
    ) where


import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty

import qualified AST.Expression as E
import qualified Type as T
import qualified TypeCheck.CaseOf as CaseOf
import TypeCheck.Check (Check)
import qualified TypeCheck.Check as Check


checkExpression :: E.Expr -> Check T.Type
checkExpression expr =
    case expr of
        E.Value v ->
            checkValue v


        E.Reference r ->
            Check.lookupVariable r


        E.If { E.condition , E.whenTrue , E.whenFalse } -> do
            conditionType <- checkExpression condition
            whenTrueType <- checkExpression whenTrue
            whenFalseType <- checkExpression whenFalse

            if conditionType /= T.Bool then
                Check.fail <| Check.IfConditionMustBeBool condition

            else if whenTrueType /= whenFalseType then
                Check.BothIfExpressionsMustHaveSameType whenTrue whenFalse
                    |> Check.fail

            else
                let
                    sharedReturnType = whenTrueType
                in
                return sharedReturnType


        E.LetIn { E.definitions, E.body } -> do
            withDefinitions definitions (checkExpression body)


        E.CaseOf { E.element, E.cases } ->
            CaseOf.checkCaseOf checkExpression element cases


        E.Lambda { E.params, E.body } -> do
            paramsWithType <-
                params
                    |> NonEmpty.toList
                    |> traverse Check.untypedVariable


            bodyType <-
                Check.withVariables paramsWithType <| checkExpression body

            paramsWithType
                |> map snd
                |> List.foldl
                    (\lambdaType paramType ->
                        T.FunctionType paramType lambdaType
                            |> T.Function
                    )
                    bodyType
                |> return


        E.Application { E.functionName, E.args } -> do
            referenceType <- Check.lookupVariable functionName
            case referenceType of
                T.Function _ ->
                    checkArguments referenceType args

                _ ->
                    Check.fail <| Check.NotFunction referenceType


checkArguments :: T.Type -> NonEmpty E.Argument -> Check T.Type
checkArguments functionType arguments =
    List.foldl
        (\fType arg -> do
            type_ <- fType
            case type_ of
                T.Function (T.FunctionType nextParamType b) -> do
                    argType <- argumentType arg
                    if argType == nextParamType then
                        return b

                    else do
                        unifyArgument arg nextParamType argType
                        return b
                _ ->
                    Check.TooManyArguments functionType arguments
                        |> Check.fail
        )
        (return functionType)
        arguments


unifyArgument :: E.Argument -> T.Type -> T.Type -> Check ()
unifyArgument arg functionParamType argType =
    case (functionParamType, argType) of
        (T.Variable n, x) -> do
            Check.unifyTypeVariable n x

        (x, T.Variable n) -> do
            Check.unifyTypeVariable n x

        _ ->
            Check.fail <| Check.WrongArgumentType functionParamType argType arg


argumentType :: E.Argument -> Check T.Type
argumentType arg =
    case arg of
        E.ValueArgument v ->
            checkValue v

        E.ReferenceArgument r ->
            Check.lookupVariable r

        E.ExpressionArgument e ->
            checkExpression e


withDefinitions
    :: NonEmpty E.Definition -> Check T.Type -> Check T.Type
withDefinitions definitions innerChecker =
    let
        toReference definition=
            case definition of
                E.SimpleDefinition id expr -> do
                    type_ <- checkExpression expr
                    return (id, type_)
    in do
    references <-
        definitions
            |> traverse toReference
            |> map NonEmpty.toList
    Check.withVariables references innerChecker


checkValue :: E.Value -> Check T.Type
checkValue v =
    case v of
        E.Bool _ ->
            return T.Bool

        E.Char _ ->
            return T.Char

        E.Float _ ->
            return T.Float

        E.Int _ ->
            return T.Int

        E.String _ ->
            return T.String
