module Type.Constraint.CaseOf (gather) where

import qualified Data.List as List
import qualified Data.Tuple as Tuple
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty

import qualified AST.Expression as E
import qualified Type as T
import Type.Constraint.Gatherer (Gatherer)
import qualified Type.Constraint.Gatherer as Gatherer
import qualified Utils.List as List


gather
    :: (E.Expr -> Gatherer T.Type)
    -> E.Expr
    -> NonEmpty E.Case
    -> Gatherer T.Type
gather gatherExpression element cases = do
    elementType <- gatherExpression element
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
        Gatherer.fail <| Gatherer.PatternMismatch element badPatterns

    else if not haveAllBodiesTheSameType then
        cases
            |> map E.caseBody
            |> Gatherer.AllCasesMustHaveSameType
            |> Gatherer.fail

    else
        return <| NonEmpty.head bodyTypes


checkPattern :: E.Expr -> E.Pattern -> Gatherer T.Type
checkPattern expression pattern =
    case pattern of
        E.WildCardPattern ->
            Gatherer.fail <| Gatherer.TODO expression

        _ ->
            Gatherer.fail <| Gatherer.TODO expression