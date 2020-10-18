module Lib
    ( someFunc
    ) where

import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import Data.Text (Text)
import qualified System.Directory as Directory
import System.IO (readFile)

import Debug (printAll)
import Expression
import Module
import qualified Type as T
import Parser.Parser (Parser)
import qualified Parser.Parser as Parser
import qualified Parser.Value as Value
import qualified Printer
import qualified TypeChecker
import qualified Utils.Either as Either


-- TopLevel

moduleParser :: Parser Module
moduleParser =
    Parser.many moduleLevel
        |> map Module


moduleLevel :: Parser TopLevel
moduleLevel = do
    Parser.oneOf
        [ function
        ]


-- FUNCTION DEFINITION


function :: Parser TopLevel
function = do
    Parser.topLevel

    typeLine <-
        Parser.maybe <| do
            functionName <- Parser.identifier
            Parser.reservedOperator ":"
            type_ <- typeParser
            return (functionName, type_)

    functionName <- Parser.identifier
    params <- Parser.many Parser.identifier
    Parser.reservedOperator "="

    body <- Parser.withPositionReference expression

    case typeLine of
        Just (typeLineName, type_) ->
            if typeLineName == functionName then
                return <| Function (Just type_) functionName params body
            else
                fail <|
                    "The function signature for '"
                        ++ typeLineName
                        ++ "' needs a body"

        Nothing ->
            return <| Function Nothing functionName params body


typeParser :: Parser T.Type
typeParser =
    let
        functionTypeParser =
            Parser.unconsumeOnFailure <|
                do -- careful not to start with typeParser.
                   -- It creates an infinite loop =/
                    a <- simpleTypeParser
                    Parser.reservedOperator "->"
                    b <- typeParser
                    return <| T.Function a b
    in
    Parser.oneOf [ functionTypeParser, simpleTypeParser ]


simpleTypeParser :: Parser T.Type
simpleTypeParser =
    Parser.oneOf
        [ map (const T.Int) <| Parser.reserved "Int"
        , map (const T.Float) <| Parser.reserved "Float"
        , map (const T.Char) <| Parser.reserved "Char"
        , map (const T.String) <| Parser.reserved "String"
        , do
            Parser.reservedOperator "("
            t <- typeParser
            Parser.reservedOperator ")"
            return t
        ]


-- EXPRESSION


expression :: Parser Expr
expression =
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
    expr <- expression
    Parser.reservedOperator ")"
    return expr


-- LAMBDA


lambda :: Parser Expr
lambda = do
    Parser.reservedOperator "\\"
    params <- Parser.atLeastOne Parser.identifier
    Parser.reservedOperator "->"
    expr <- expression
    return <| Lambda params expr



-- IF THEN ELSE


ifThenElse :: Parser Expr
ifThenElse = do
    condition <-
        Parser.between
            (Parser.reserved "if")
            (Parser.reserved "then")
            (expression)
    whenTrue <- expression
    Parser.reserved "else"
    whenFalse <- expression
    return (If condition whenTrue whenFalse)


-- LET IN


letIn :: Parser Expr
letIn = do
    definitions <-
        Parser.between
            (Parser.reserved "let")
            (Parser.reserved "in")
            (Parser.atLeastOne definition)
    expr <- expression
    return <| LetIn definitions expr


definition :: Parser Definition
definition = do
    id <- Parser.identifier
    Parser.reservedOperator "="
    expr <- Parser.withPositionReference expression
    return <| SimpleDefinition id expr



-- CASE OF


caseOf :: Parser Expr
caseOf = Parser.withPositionReference <|
    do
        expr <-
            Parser.between
                (Parser.reserved "case")
                (Parser.reserved "of")
                (expression)

        cases <-
            Parser.atLeastOne <| do
                Parser.indented
                caseParser

        return <| CaseOf expr cases


caseParser :: Parser Case
caseParser = do
    pattern <- patternParser
    Parser.reservedOperator "->"
    expr <- Parser.withPositionReference expression
    return <| Case pattern expr



patternParser :: Parser Pattern
patternParser =
    Parser.oneOf
         [ map ValuePattern Value.valueParser
         , map IdentifierPattern Parser.identifier
         , map (const WildCardPattern) <| Parser.reservedOperator "_"
         ]



-- MAIN


someFunc :: IO ()
someFunc = do
    dir <- Directory.getCurrentDirectory
    putStrLn dir

    let filePath = "test/test"
    let parser = moduleParser

    file <- readFile filePath
    putStrLn file
    let parsed = Parser.runParser parser filePath file

    putStrLn <| "\n\n--- RESULT ---\n"
    putStrLn <| Either.fold show Printer.printModule parsed
    --putStrLn $ show parsed

    putStrLn <| "\n\n--- TYPE CHECK ---\n"
    parsed
        |> map TypeChecker.check
        |> map TypeChecker.showCheckResult
        |> Either.fold (const "FAIL") identity
        |> putStrLn
