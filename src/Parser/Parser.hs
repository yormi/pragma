module Parser.Parser
    ( FieldError(..)
    , Parser
    , ParserError(..)
    , RecordError(..)
    , TypeAliasError(..)
    , atLeastOne
    , between
    , catchRaw
    , charLiteral
    , constructorIdentifier
    , dataIdentifier
    , endOfFile
    , endPosition
    , fail
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
    , runParser
    , sameLine
    , sameLineOrIndented
    , stringLiteral
    , toParserError
    , topLevel
    , typeIdentifier
    , typeVariableIdentifier
    , unconsumeOnFailure
    , unconsumeOnFailure_
    , withPositionReference
    ) where


import Control.Monad.Trans.Except (Except)
import qualified Control.Monad.Trans.Except as Except
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as Aeson
import qualified Data.Char as Char
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Text.Parsec as Parsec
import qualified Text.Parsec.Error as ParserError
import qualified Text.Parsec.Indent as Indent
import qualified Text.Parsec.Token as Token

import AST.CodeQuote (CodeQuote, Position(..))
import qualified AST.CodeQuote as CodeQuote
import AST.Identifier
    ( ConstructorId
    , DataId
    , ReferenceId
    , TypeId
    , TypeVariableId
    )
import qualified AST.Identifier as Identifier
import qualified Utils.Either as Either
import qualified Utils.List as List
import qualified Utils.Maybe as Maybe
import qualified Utils.String as String


-- Using this complex form because type synonym can't be partially applied.
-- We need partial application for abstracting the `a` away from
-- `IndentT s u m a` in order to make
-- `IndentT` a kind `* -> *` for `ParsecT` to accept
type Parser a
    = Parsec.ParsecT String FileContent
        (Indent.IndentT
            (Except ParserError)
        )
        a


type FileContent
    = String


data ParserError
    = RawError String
    | SumTypeConstructorMustStartWithUpper CodeQuote
    | DataNameMustStartWithLowerCase CodeQuote
    | TypeNameMustStartWithUpperCase CodeQuote
    | TypeVariableMustStartWithLowerCase CodeQuote
    | TypeSignatureNameMismatch CodeQuote DataId DataId
    | FunctionMustHaveTypeSignature CodeQuote
    | FieldInvalid FieldError Position
    | RecordInvalid RecordError Position
    | TypeAliasInvalid TypeAliasError Position
    deriving (Eq, Generic, Show, FromJSON, ToJSON)


data RecordError
    = TrailingCharacter
    | ExtraComma
    deriving (Eq, Generic, Show, FromJSON, ToJSON)


data FieldError
    = DefinitionMustUseColon
    | MustHaveTypeAnnotation
    deriving (Eq, Generic, Show, FromJSON, ToJSON)


data TypeAliasError
    = TypeNameInvalid
    | TypeVariableInvalid
    deriving (Eq, Generic, Show, FromJSON, ToJSON)


fail :: ParserError -> Parser a
fail =
    lift << lift << Except.throwE


toParserError :: Parsec.ParseError -> [ParserError]
toParserError error =
    ParserError.errorMessages error
        |> map ParserError.messageString
        |> map encodeUtf8
        |> map Aeson.decode
        |> map (Maybe.withDefault (RawError <| show error))


runParser :: Parser a -> FilePath -> FileContent -> Either [ParserError] a
runParser parser filePath fileContent =
    let
        -- To allow differentiating last file char from position bumped
        -- too far after last lexeme parsing
        withNewLine =
            fileContent ++ "\n"
    in
    Indent.runIndentParserT parser withNewLine filePath withNewLine
        |> Except.runExcept
        |> Either.mapLeft List.singleton
        |> bind (Either.mapLeft toParserError)


languageDefinition :: Monad m => Token.GenLanguageDef String FileContent m
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


lexer :: Monad m => Token.GenTokenParser String FileContent m
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
    Parsec.try >> Parsec.optionMaybe


many :: Parser a -> Parser [a]
many =
    Parsec.many


manyUntil :: Parser a -> Parser b -> Parser [b]
manyUntil end p =
    Parsec.manyTill p end


atLeastOne :: Parser a -> Parser (NonEmpty a)
atLeastOne =
    Parsec.many1
        >> bind
            (NonEmpty.nonEmpty
                >> map return
                >> Maybe.withDefault
                    (Parsec.unexpected
                        "There should be at least one element to parse"
                    )
            )
        >> Parsec.try


oneOf :: [Parser a] -> Parser a
oneOf =
    Parsec.choice


between :: Parser () -> Parser () -> Parser c -> Parser c
between before after mainParser = do
    before
    x <- mainParser
    after
    return x


unconsumeOnFailure :: Parser a -> Parser a
unconsumeOnFailure parser=
    Parsec.try <| do
        parser
            |> traverse
                (\result ->
                    Except.catchE result (show >> Parsec.parserFail)
                )


hasFailed :: Parser a -> 


unconsumeOnFailure_ :: Parser a -> Parser a
unconsumeOnFailure_ =
    Parsec.try


catchRaw :: ParserError -> Parser a -> Parser a
catchRaw error parser = do
    result <- Parsec.optionMaybe parser
    case result of
        Just x ->
            return x
        Nothing ->
            fail error


endOfFile :: Parser ()
endOfFile =
    Parsec.eof


-- Position


position :: Parser Position
position =
    map
        (\sourcePosition ->
            Position
                (Parsec.sourceName sourcePosition)
                (Parsec.sourceLine sourcePosition)
                (Parsec.sourceColumn sourcePosition)
        )
        Parsec.getPosition


endPosition :: Parser Position
endPosition = do
    fileContent <- Parsec.getState
    Position filename currentLine currentColumn <- position

    let beforePosition = cutFrom currentLine currentColumn fileContent
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


withPositionReference :: Parser a -> Parser a
withPositionReference =
    Indent.withPos


indented :: Parser ()
indented =
    Indent.indented


sameLine :: Parser ()
sameLine =
    Indent.same


sameLineOrIndented :: Parser ()
sameLineOrIndented =
    Indent.sameOrIndented
