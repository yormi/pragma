module Parser3.Expression
    ( expressionParser
    ) where

import AST3.Expression (Expression)
import qualified AST3.Expression as Expression
import Parser3.Parser (Parser)
import qualified Parser3.Combinator as C
import qualified Parser3.Lexeme as Lexeme
import qualified Parser3.Identifier as Identifier
import qualified Parser3.Indentation as Indentation
import qualified Parser3.Parser as Parser
import qualified Parser3.Model.Quote as Quote
import qualified Parser3.Value as Value


expressionParser :: Parser Expression
expressionParser =
    C.oneOf
        [ parenthesizedExpression
        , map Expression.Value Value.parser
        -- , caseOf
        , ifThenElse
        , letIn
        , Parser.unconsumeOnFailure application
        , reference
        -- , lambda
        ]



-- REFERENCE


reference :: Parser Expression
reference = do
    Identifier.reference
        |> map Expression.Reference



-- APPLICATION


application :: Parser Expression
application = do
    functionName <- Identifier.reference
    args <-
        C.atLeastOne <| do
            C.someSpace
            Indentation.sameLineOrIndented
            argument

    Expression.Application functionName args
        |> return


argument :: Parser Expression
argument = do
    C.someSpace
    C.oneOf
        [ map Expression.Value Value.parser
        , reference
        , parenthesizedExpression
        ]


parenthesizedExpression :: Parser Expression
parenthesizedExpression =
    Lexeme.parenthesized expressionParser



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



-- IF THEN ELSE


ifThenElse :: Parser Expression
ifThenElse = do
    ifQuote <- Lexeme.reserved "if"
    condition <- expressionParser
    _ <- Lexeme.reserved "then"
    whenTrue <- expressionParser
    _ <- Lexeme.reserved "else"
    whenFalse <- expressionParser

    Expression.If
        (Quote.from ifQuote)
        condition
        whenTrue
        whenFalse
        |> return



-- LET IN


letIn :: Parser Expression
letIn = do
    C.someSpace
    from <- Parser.getPosition
    _ <- Lexeme.reserved "let"
    definitions <- C.atLeastOne definition
    _ <- Lexeme.reserved "in"
    expr <- expressionParser
    return <| Expression.LetIn from definitions expr


definition :: Parser Expression.Definition
definition = do
    id <- Identifier.data_
    _ <- Lexeme.operator "="
    expr <- Indentation.withPositionReference expressionParser
    return <| Expression.SimpleDefinition id expr



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
