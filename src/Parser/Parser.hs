module Parser.Parser
    ( Parser
    , ParserError(..)
    , atLeastOne
    , between
    , charLiteral
    , endOfFile
    , fail
    , identifier
    , indented
    , many
    , maybe
    , numberLiteral
    , position
    , oneOf
    , reserved
    , reservedOperator
    , runParser
    , sameLineOrIndented
    , stringLiteral
    , topLevel
    , unconsumeOnFailure
    , withPositionReference
    ) where


import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Text.Parsec as Parser
import qualified Text.Parsec.Indent as Indent
import qualified Text.Parsec.Token as Token


import AST.CodeQuote (CodeQuote, Position(..))
import qualified Utils.Either as Either
import qualified Utils.Maybe as Maybe


type Parser a = Indent.IndentParserT String () (Either ParserError) a

-- type ParserError = Parser.ParseError
data ParserError
    = RawError Parser.ParseError
    | SumTypeConstructorMustStartWithUpper CodeQuote
    deriving (Eq, Show)


fail :: ParserError -> Parser a
fail =
    lift << lift << Left


runParser :: Parser a -> FilePath -> String -> Either ParserError a
runParser parser filePath file = do
    Indent.runIndentParserT parser () filePath file
        |> map (Either.mapLeft RawError)
        |> join


languageDefinition :: Monad m => Token.GenLanguageDef String () m
languageDefinition = Token.LanguageDef
  { Token.commentStart    = "{-"
  , Token.commentEnd      = "-}"
  , Token.commentLine     = "--"
  , Token.nestedComments  = True
  , Token.identStart      = Parser.letter
  , Token.identLetter     = Parser.choice [ Parser.alphaNum, Parser.oneOf "_'" ]
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


charLiteral :: Parser Char
charLiteral =
    Token.charLiteral lexer


numberLiteral :: Parser (Either Integer Double)
numberLiteral =
    Token.naturalOrFloat lexer


stringLiteral :: Parser String
stringLiteral =
    Token.stringLiteral lexer


maybe :: Parser a -> Parser (Maybe a)
maybe =
    Parser.optionMaybe << Parser.try


many :: Parser a -> Parser [a]
many =
    Parser.many


atLeastOne :: Parser a -> Parser (NonEmpty a)
atLeastOne =
    Parser.many1
        >> bind
            (NonEmpty.nonEmpty
                >> map return
                >> Maybe.withDefault
                    (Parser.unexpected
                        "There should be at least one element to parse"
                    )
            )
        >> Parser.try


oneOf :: [Parser a] -> Parser a
oneOf =
    Parser.choice


between :: Parser () -> Parser () -> Parser c -> Parser c
between before after mainParser = do
    before
    x <- mainParser
    after
    return x


unconsumeOnFailure :: Parser a -> Parser a
unconsumeOnFailure =
    Parser.try


endOfFile :: Parser ()
endOfFile =
    Parser.eof


-- Position


position :: Parser Position
position =
    map
        (\sourcePosition ->
            Position
                (Parser.sourceName sourcePosition)
                (Parser.sourceLine sourcePosition)
                (Parser.sourceColumn sourcePosition)
        )
        Parser.getPosition


-- Indentation

topLevel :: Parser ()
topLevel =
    Indent.topLevel


withPositionReference :: Parser a -> Parser a
withPositionReference =
    Indent.withPos


indented :: Parser ()
indented =
    Indent.indented


sameLineOrIndented :: Parser ()
sameLineOrIndented =
    Indent.sameOrIndented
