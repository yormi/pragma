module Parser.AST.Expression
    ( expressionParser
    ) where

import AST.Expression (Expression)
import qualified AST.Expression as Expression
import Parser.Parser (Parser)
import qualified Parser.Combinator as C
import qualified Parser.Lexeme as Lexeme
import qualified Parser.AST.Identifier as Identifier
import qualified Parser.Indentation as Indentation
import qualified Parser.Parser as Parser
import qualified Parser.Model.Quote as Quote
import qualified Parser.AST.Value as Value


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
        , lambda
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



-- LAMBDA


lambda :: Parser Expression
lambda = do
    quote <- Lexeme.operator "\\"
    params <- C.atLeastOne Identifier.data_
    _ <- Lexeme.operator "->"
    expr <- expressionParser
    return <| Expression.Lambda (Quote.from quote) params expr



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
--             Quote.fromPositions from to
--                 |> (\q -> QuotedExpression q parsed)
--         )
--         Parser.position
--         parser
--         Parser.position