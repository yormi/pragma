module Parser.Expression
    ( expressionParser
    ) where

import AST.Expression
import Parser.Parser (Parser)
import qualified Parser.Parser as Parser
import qualified Parser.Value as Value


expressionParser :: Parser Expr
expressionParser =
    Parser.oneOf
        [ parenthesisedExpression
        , caseOf
        , ifThenElse
        , letIn
        , application
        , map Reference Parser.identifier
        , map Value Value.valueParser
        , lambda
        ]


-- APPLICATION


application :: Parser Expr
application =
    Parser.unconsumeOnFailure <| do
        functionName <- Parser.identifier
        args <-
            Parser.atLeastOne <| do
                Parser.sameLineOrIndented
                argument
        return <| Application functionName args


argument :: Parser Argument
argument =
    Parser.oneOf
        [ map ValueArgument Value.valueParser
        , map ReferenceArgument Parser.identifier
        , map ExpressionArgument parenthesisedExpression
        ]


parenthesisedExpression :: Parser Expr
parenthesisedExpression = do
    Parser.reservedOperator "("
    expr <- expressionParser
    Parser.reservedOperator ")"
    return expr


-- LAMBDA


lambda :: Parser Expr
lambda = do
    Parser.reservedOperator "\\"
    params <- Parser.atLeastOne Parser.identifier
    Parser.reservedOperator "->"
    expr <- expressionParser
    return <| Lambda params expr



-- IF THEN ELSE


ifThenElse :: Parser Expr
ifThenElse = do
    condition <-
        Parser.between
            (Parser.reserved "if")
            (Parser.reserved "then")
            (expressionParser)
    whenTrue <- expressionParser
    Parser.reserved "else"
    whenFalse <- expressionParser
    return (If condition whenTrue whenFalse)


-- LET IN


letIn :: Parser Expr
letIn = do
    definitions <-
        Parser.between
            (Parser.reserved "let")
            (Parser.reserved "in")
            (Parser.atLeastOne definition)
    expr <- expressionParser
    return <| LetIn definitions expr


definition :: Parser Definition
definition = do
    id <- Parser.identifier
    Parser.reservedOperator "="
    expr <- Parser.withPositionReference expressionParser
    return <| SimpleDefinition id expr



-- CASE OF


caseOf :: Parser Expr
caseOf = Parser.withPositionReference <|
    do
        expr <-
            Parser.between
                (Parser.reserved "case")
                (Parser.reserved "of")
                (expressionParser)

        cases <-
            Parser.atLeastOne <| do
                Parser.indented
                caseParser

        return <| CaseOf expr cases


caseParser :: Parser Case
caseParser = do
    pattern <- patternParser
    Parser.reservedOperator "->"
    expr <- Parser.withPositionReference expressionParser
    return <| Case pattern expr



patternParser :: Parser Pattern
patternParser =
    Parser.oneOf
         [ map ValuePattern Value.valueParser
         , map IdentifierPattern Parser.identifier
         , map (const WildCardPattern) <| Parser.reservedOperator "_"
         ]
