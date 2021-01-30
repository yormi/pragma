module Parser2.Expression
    ( expressionParser
    ) where

import AST.Expression
import Parser2.Parser (QuotedParser)
import qualified Parser2.Combinator as C
import qualified Parser2.Error as E
import qualified Parser2.Identifier as Identifier
import qualified Parser2.Parser as P
-- import qualified Parser2.Value as Value


expressionParser :: QuotedParser Expression
expressionParser =
    C.oneOf E.ExpressionExpected
        -- [ parenthesisedExpression
        -- , caseOf
        -- , ifThenElse
        -- , letIn
        -- , application
        [ reference
        -- , map Value Value.valueParser
        --     |> exprParser
        -- , lambda
        ]


-- REFERENCE


reference :: QuotedParser Expression
reference =
    Identifier.reference
        |> P.mapResult Reference


-- -- APPLICATION
-- 
-- 
-- application :: Parser QuotedExpression
-- application =
--     (do
--         functionName <- Parser.referenceIdentifier
--         args <-
--             Parser.atLeastOne <| do
--                 Parser.sameLineOrIndented
--                 argument
--         Application functionName args
--             |> return
--     )
--         |> exprParser
--         |> Parser.unconsumeOnFailure
-- 
-- 
-- argument :: Parser QuotedExpression
-- argument =
--     Parser.oneOf
--         [ map Value Value.valueParser
--             |> exprParser
--         , reference
--         , parenthesisedExpression
--         ]
-- 
-- 
-- parenthesisedExpression :: Parser QuotedExpression
-- parenthesisedExpression = do
--     Parser.reservedOperator "("
--     expr <- expressionParser
--     Parser.reservedOperator ")"
--     return expr
-- 
-- 
-- -- LAMBDA
-- 
-- 
-- lambda :: Parser QuotedExpression
-- lambda =
--     (do
--         Parser.reservedOperator "\\"
--         params <- Parser.atLeastOne Parser.dataIdentifier
--         Parser.reservedOperator "->"
--         expr <- expressionParser
--         return <| Lambda params expr
--     )
--     |> exprParser
-- 
-- 
-- 
-- -- IF THEN ELSE
-- 
-- 
-- ifThenElse :: Parser QuotedExpression
-- ifThenElse =
--     (do
--         condition <-
--             Parser.between
--                 (Parser.reserved "if")
--                 (Parser.reserved "then")
--                 (expressionParser)
--         whenTrue <- expressionParser
--         Parser.reserved "else"
--         whenFalse <- expressionParser
-- 
--         If condition whenTrue whenFalse
--             |> return
--     )
--         |> exprParser
-- 
-- 
-- -- LET IN
-- 
-- 
-- letIn :: Parser QuotedExpression
-- letIn =
--     (do
--         definitions <-
--             Parser.between
--                 (Parser.reserved "let")
--                 (Parser.reserved "in")
--                 (Parser.atLeastOne definition)
--         expr <- expressionParser
--         return <| LetIn definitions expr
--     )
--         |> exprParser
-- 
-- 
-- definition :: Parser Definition
-- definition = do
--     id <- Parser.dataIdentifier
--     Parser.reservedOperator "="
--     expr <- Parser.withPositionReference expressionParser
--     return <| SimpleDefinition id expr
-- 
-- 
-- 
-- -- CASE OF
-- 
-- 
-- caseOf :: Parser QuotedExpression
-- caseOf =
--     ( do
--         expr <-
--             Parser.between
--                 (Parser.reserved "case")
--                 (Parser.reserved "of")
--                 (expressionParser)
-- 
--         cases <-
--             Parser.atLeastOne <| do
--                 Parser.indented
--                 caseParser
-- 
--         return <| CaseOf expr cases
--     )
--         |> exprParser
--         |> Parser.withPositionReference
-- 
-- 
-- caseParser :: Parser Case
-- caseParser = do
--     pattern <- patternParser
--     Parser.reservedOperator "->"
--     expr <- Parser.withPositionReference expressionParser
--     return <| Case pattern expr
-- 
-- 
-- 
-- patternParser :: Parser Pattern
-- patternParser =
--     Parser.oneOf
--          [ map ValuePattern Value.valueParser
--          , map IdentifierPattern Parser.dataIdentifier
--          , map (const WildCardPattern) <| Parser.reservedOperator "_"
--          ]
-- 
-- 
-- exprParser :: Parser Expression -> Parser QuotedExpression
-- exprParser parser =
--     map3
--         (\from parsed to ->
--             CodeQuote.fromPositions from to
--                 |> (\q -> QuotedExpression q parsed)
--         )
--         Parser.position
--         parser
--         Parser.position
