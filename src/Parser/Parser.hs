module Parser.Parser
    ( atLeastOne
    , between
    , charLiteral
    , constructorIdentifier
    , dataIdentifier
    , endOfFile
    , endPosition
    , identifier
    , indented
    , many
    , manyUntil
    , maybe
    , numberLiteral
    , position
    , oneOf
    , referenceIdentifier
    , reserved
    , reservedOperator
    , sameLine
    , sameLineOrIndented
    , stringLiteral
    , topLevel
    , typeIdentifier
    , typeVariableIdentifier
    , unconsumeOnFailure
    , withPositionReference
    ) where


import qualified Control.Monad as Monad
import qualified Data.Aeson as Aeson
import qualified Data.Char as Char
import qualified Data.Functor.Identity as Identity
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Text.Parsec as Parsec
import qualified Text.Parsec.Error as ParserError
import qualified Text.Parsec.Indent as Indent
import qualified Text.Parsec.Token as Token

import AST.CodeQuote (Position(..))
import qualified AST.CodeQuote as CodeQuote
import AST.Identifier
    ( ConstructorId
    , DataId
    , ReferenceId
    , TypeId
    , TypeVariableId
    )
import qualified AST.Identifier as Identifier
import Parser.Error
import Parser.Model
import qualified Utils.Either as Either
import qualified Utils.List as List
import qualified Utils.Maybe as Maybe
import qualified Utils.NonEmpty as NonEmpty
import qualified Utils.String as String


languageDefinition :: Monad m => Token.GenLanguageDef String State m
languageDefinition = Token.LanguageDef
  { Token.commentStart    = "{-"
  , Token.commentEnd      = "-}"
  , Token.commentLine     = "--"
  , Token.nestedComments  = True
  , Token.identStart      = Parsec.letter
  , Token.identLetter     = Parsec.choice [ Parsec.alphaNum, Parsec.oneOf "_'" ]
  , Token.opStart         = Parsec.oneOf "=!+-*/><|\\:"
  , Token.opLetter        = Parsec.oneOf "=!+-*/><|\\:"
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


lexer :: Monad m => Token.GenTokenParser String State m
lexer =
    Token.makeTokenParser languageDefinition


constructorIdentifier :: Parser ConstructorId
constructorIdentifier = do
    from <- position
    id <- identifier
    to <- position
    let codeQuote = CodeQuote.fromPositions from to
    case Identifier.constructorId id of
        Just constructorId ->
            return constructorId

        Nothing ->
            fail <| SumTypeConstructorMustStartWithUpper codeQuote


dataIdentifier :: Parser DataId
dataIdentifier = do
    from <- position
    id <- identifier
    to <- position
    let codeQuote = CodeQuote.fromPositions from to
    case Identifier.dataId id of
        Just dataId ->
            return dataId

        Nothing ->
            fail <| DataNameMustStartWithLowerCase codeQuote



referenceIdentifier :: Parser ReferenceId
referenceIdentifier = do
    identifier
        |> map Identifier.referenceId


typeIdentifier :: Parser TypeId
typeIdentifier = do
    from <- position
    id <- identifier
    to <- position
    let codeQuote = CodeQuote.fromPositions from to
    case Identifier.typeId id of
        Just typeId ->
            return typeId

        Nothing ->
            fail <| TypeNameMustStartWithUpperCase codeQuote


typeVariableIdentifier :: Parser TypeVariableId
typeVariableIdentifier = do
    from <- position
    id <- identifier
    to <- position
    let codeQuote = CodeQuote.fromPositions from to
    case Identifier.typeVariableId id of
        Just variableId ->
            return variableId

        Nothing ->
            fail <| TypeVariableMustStartWithLowerCase codeQuote


toParser :: Indent.IndentParser String State a -> Parser a
toParser =
    map Right >> Parser


identifier :: Parser String
identifier =
    Token.identifier lexer
        |> toParser


reserved :: String -> Parser ()
reserved =
    Token.reserved lexer
        >> toParser


reservedOperator :: String -> Parser ()
reservedOperator =
    Token.reservedOp lexer
        >> toParser


charLiteral :: Parser Char
charLiteral =
    Token.charLiteral lexer
        |> toParser


numberLiteral :: Parser (Either Integer Double)
numberLiteral =
    Token.naturalOrFloat lexer
        |> toParser


stringLiteral :: Parser String
stringLiteral =
    Token.stringLiteral lexer
        |> toParser


maybe :: Parser a -> Parser (Maybe a)
maybe (Parser p) =
    Parsec.try p
        |> map Either.toMaybe
        |> Parsec.optionMaybe
        |> map join
        |> toParser


many :: Parser a -> Parser [a]
many (Parser p) =
    Parsec.many p
        |> map sequence
        |> Parser


manyUntil :: Parser b -> Parser a -> Parser [a]
manyUntil (Parser end) (Parser p) =
    Parsec.manyTill p end
        |> map sequence
        |> Parser


atLeastOne :: Parser a -> Parser (NonEmpty a)
atLeastOne (Parser a) =
    (do
        either <-
            Parsec.many1 a
                |> map sequence

        Parsec.try <|
            case either of
                Right (x : rest) ->
                    NonEmpty.build x rest
                        |> Right
                        |> return

                Right [] ->
                    Parsec.unexpected "There should be at least one element to parse"

                Left e ->
                    return <| Left e
    )
        |> Parser


oneOf :: [Parser a] -> Parser a
oneOf parsers =
    parsers
        |> map (\(Parser p) -> p)
        |> Parsec.choice
        |> Parser


between :: Parser () -> Parser () -> Parser c -> Parser c
between before after mainParser = do
    before
    x <- mainParser
    after
    return x


unconsumeOnFailure :: Parser a -> Parser a
unconsumeOnFailure (Parser p) =
    Parsec.try p
        |> Parser


endOfFile :: Parser ()
endOfFile =
    Parsec.eof
        |> toParser


-- Position


position :: Parser Position
position =
    Parsec.getPosition
        |> map
            (\sourcePosition ->
                Position
                    (Parsec.sourceName sourcePosition)
                    (Parsec.sourceLine sourcePosition)
                    (Parsec.sourceColumn sourcePosition)
            )
        |> toParser


endPosition :: Parser Position
endPosition = do
    state <- getState
    let fileContentWithNewLine =
        -- To allow differentiating last file char from position bumped
        -- too far after last lexeme parsing
            fileContent state ++ "\n"
    Position filename currentLine currentColumn <- position

    let beforePosition =
            cutFrom currentLine currentColumn fileContentWithNewLine
    beforePosition
        |> List.dropWhileEnd (not << Char.isSpace)
        |> List.dropWhileEnd Char.isSpace
        |> (\str ->
                Position
                    filename
                    (countLine str)
                    (countColumn str)
            )
        |> return


cutFrom :: Int -> Int -> String -> String
cutFrom lineNumber columnNumber =
    String.splitLines
        >> (\lines -> lines ++ [ "" ])
        >> List.indexedMap
            (\index line ->
                if index + 1 < lineNumber then
                    Just line

                else if index + 1 == lineNumber then
                    List.take columnNumber line
                        |> Just

                else
                    Nothing
            )
        >> Maybe.values
        >> String.mergeLines


countLine :: String -> Int
countLine =
        String.splitLines >> List.length


countColumn :: String -> Int
countColumn =
    String.splitLines
        >> List.last
        >> map List.length
        >> Maybe.withDefault 1


-- Indentation

topLevel :: Parser ()
topLevel =
    Indent.topLevel
        |> toParser


withPositionReference :: Parser a -> Parser a
withPositionReference (Parser p) =
    Indent.withPos p
        |> Parser



indented :: Parser ()
indented =
    Indent.indented
        |> toParser


sameLine :: Parser ()
sameLine =
    Indent.same
        |> toParser


sameLineOrIndented :: Parser ()
sameLineOrIndented =
    Indent.sameOrIndented
        |> toParser
