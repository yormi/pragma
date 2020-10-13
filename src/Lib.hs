module Lib
    ( someFunc
    ) where

import Data.Bifunctor (bimap)
import Data.Functor ((<&>))
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import Data.Text (Text)
import Text.Parsec  ((<|>), Parsec)
import qualified Text.Parsec as Parser
import qualified Text.Parsec.Indent as Indent
import qualified Text.Parsec.Token as Token
import qualified System.Directory as Directory
import System.IO (readFile)

import Debug (printAll)
import Expression
import Module
import qualified Type as T
import Parser (Parser)
import qualified Printer
import qualified TypeChecker
import qualified Utils.Either as Either


-- PARSER



languageDefinition :: Monad m => Token.GenLanguageDef String () m
languageDefinition = Token.LanguageDef
  { Token.commentStart    = "{-"
  , Token.commentEnd      = "-}"
  , Token.commentLine     = "--"
  , Token.nestedComments  = True
  , Token.identStart      = Parser.letter
  , Token.identLetter     = Parser.alphaNum <|> Parser.oneOf "_'"
  , Token.opStart         = Parser.oneOf "=!+-*/><|\\:"
  , Token.opLetter        = Parser.oneOf "=!+-*/><|\\:"
  , Token.reservedNames   =
    [ "type", "alias"
    , "if", "then", "else"
    , "let", "in"
    , "case", "of"
    , "False", "True"
    ]
  , Token.reservedOpNames =
    [ "="
    , ":"
    , "\\", "->"
    , "_"
    , ">>", "<<", "|>", "<|"
    ]
  , Token.caseSensitive   = True
  }


lexer :: Monad m => Token.GenTokenParser String () m
lexer =
    Token.makeTokenParser languageDefinition


identifier :: Parser String
identifier =
    Token.identifier lexer


reserved :: String -> Parser ()
reserved =
    Token.reserved lexer


reservedOperator :: String -> Parser ()
reservedOperator =
    Token.reservedOp lexer


-- TopLevel

moduleParser :: Parser Module
moduleParser =
    Parser.many1 moduleLevel
        |> map Module


moduleLevel :: Parser TopLevel
moduleLevel = do
    Parser.choice
        [ function
        ]


-- FUNCTION DEFINITION


function :: Parser TopLevel
function = do
    Indent.topLevel

    typeLine <-
        Parser.optionMaybe <| Parser.try <| do
            functionName <- identifier
            reservedOperator ":"
            type_ <- typeParser
            return (functionName, type_)

    functionName <- identifier
    params <-
        Parser.many1 identifier
            |> map NonEmpty.fromList
    reservedOperator "="

    body <- Indent.withPos expression

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
            Parser.try <|
                do -- careful not to start with typeParser.
                   -- It creates an infinite loop =/
                    a <- simpleTypeParser
                    reservedOperator "->"
                    b <- typeParser
                    return <| T.Function a b
    in
    Parser.choice [ functionTypeParser, simpleTypeParser ]


simpleTypeParser :: Parser T.Type
simpleTypeParser =
    Parser.choice
        [ map (const T.Int) <| reserved "Int"
        , map (const T.Float) <| reserved "Float"
        , map (const T.Char) <| reserved "Char"
        , map (const T.String) <| reserved "String"
        , do
            reservedOperator "("
            t <- typeParser
            reservedOperator ")"
            return t
        ]


-- EXPRESSION


expression :: Parser Expr
expression =
    Parser.choice
        [ parenthesisedExpression
        , caseOf
        , ifThenElse
        , letIn
        , application
        , map Reference identifier
        , map Value value
        , lambda
        ]


-- APPLICATION


application :: Parser Expr
application =
    Parser.try <| do
        functionName <- identifier
        args <-
            map NonEmpty.fromList <| Parser.many1 <|
                do
                    Indent.sameOrIndented
                    argument
        return <| Application functionName args


argument :: Parser Argument
argument =
    Parser.choice
        [ map ValueArgument value
        , map ReferenceArgument identifier
        , map ExpressionArgument parenthesisedExpression
        ]


parenthesisedExpression :: Parser Expr
parenthesisedExpression = do
    Parser.between
        (reservedOperator "(")
        (reservedOperator ")")
        expression


-- LAMBDA


lambda :: Parser Expr
lambda = do
    reservedOperator "\\"
    params <- map NonEmpty.fromList <| Parser.many1 identifier
    reservedOperator "->"
    expr <- expression
    return <| Lambda params expr



-- VALUE


value :: Parser Value
value =
    Parser.choice
        [ map Bool bool
        , map Char <| Token.charLiteral lexer
        , number
        , map String <| Token.stringLiteral lexer
        ]


number :: Parser Value
number =
    Parser.choice
        [ do
            reservedOperator "-"
            n <- Token.naturalOrFloat lexer
            n
                |> bimap ((*) (-1)) ((*) (-1))
                |> Either.fold Int Float
                |> return

        , Token.naturalOrFloat lexer
            |> map (Either.fold Int Float)
        ]


bool :: Parser BoolLiteral
bool =
    Parser.choice
        [ map (const TrueLiteral) <| reserved "True"
        , map (const FalseLiteral) <| reserved "False"
        ]


-- IF THEN ELSE


ifThenElse :: Parser Expr
ifThenElse = do
    condition <-
        Parser.between
            (reserved "if")
            (reserved "then")
            expression
    whenTrue <- expression
    reserved "else"
    whenFalse <- expression
    return (If condition whenTrue whenFalse)


-- LET IN


letIn :: Parser Expr
letIn = do
    reserved "let"
    definitions <- map NonEmpty.nonEmpty <| Parser.many1 definition
    reserved "in"
    expr <- expression

    case definitions of
        Just defs ->
            return <| LetIn defs expr

        Nothing ->
            fail "There is no definitions in the let expression"


definition :: Parser Definition
definition = do
    id <- identifier
    reservedOperator "="
    expr <- Indent.withPos expression
    return <| SimpleDefinition id expr



-- CASE OF


caseOf :: Parser Expr
caseOf = Indent.withPos <|
    do
        expr <- Parser.between (reserved "case") (reserved "of") expression
        cases <-
            (do
                Indent.indented
                caseParser
            )
                |> Parser.many1
                |> map NonEmpty.nonEmpty
        case cases of
            Just cs ->
                return <| CaseOf expr cs

            Nothing ->
                fail "The case..of structure needs at least 1 case"


caseParser :: Parser Case
caseParser = do
    pattern <- patternParser
    reservedOperator "->"
    expr <- Indent.withPos expression
    return <| Case pattern expr



patternParser :: Parser Pattern
patternParser =
    Parser.choice
         [ map ValuePattern value
         , map IdentifierPattern identifier
         , map (const WildCardPattern) <| reservedOperator "_"
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
    let parsed = Indent.runIndentParser parser () filePath file

    putStrLn <| "\n\n--- RESULT ---\n"
    putStrLn <| Either.fold show Printer.printModule parsed
    --putStrLn $ show parsed

    putStrLn <| "\n\n--- TYPE CHECK ---\n"
    parsed
        |> map TypeChecker.check
        |> map TypeChecker.showCheckResult
        |> Either.fold (const "FAIL") identity
        |> putStrLn
