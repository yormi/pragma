module TypeCheck.CaseOf (checkCaseOf) where

import qualified Data.List as List
import qualified Data.Tuple as Tuple
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty

import qualified AST.Expression as E
import qualified Type as T
import TypeCheck.Check (Check)
import qualified TypeCheck.Check as Check
import qualified Utils.List as List


checkCaseOf
    :: (E.Expr -> Check T.Type)
    -> E.Expr
    -> NonEmpty E.Case
    -> Check T.Type
checkCaseOf checkExpression element cases = do
    elementType <- checkExpression element
    badPatterns <-
            cases
                |> traverse
                    (\c -> do
                        let pattern = E.pattern c
                        t <- checkPattern (E.CaseOf element cases) pattern
                        return (t, pattern)
                    )
                |> map
                    (NonEmpty.filter (\(t, _) -> t == elementType)
                        >> map Tuple.snd
                    )

    bodyTypes <- traverse (E.caseBody >> checkExpression) cases
    let haveAllBodiesTheSameType =
            bodyTypes
                |> NonEmpty.toList
                |> List.all (\t -> t == NonEmpty.head bodyTypes)

    if not <| List.isEmpty badPatterns then
        Check.fail <| Check.PatternMismatch element badPatterns

    else if not haveAllBodiesTheSameType then
        cases
            |> map E.caseBody
            |> Check.AllCasesMustHaveSameType
            |> Check.fail

    else
        return <| NonEmpty.head bodyTypes


checkPattern :: E.Expr -> E.Pattern -> Check T.Type
checkPattern expression pattern =
    case pattern of
        E.WildCardPattern ->
            Check.fail <| Check.TODO expression

        _ ->
            Check.fail <| Check.TODO expression
