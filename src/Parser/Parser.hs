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


import Control.Monad.Trans.Except (ExceptT(..))
import qualified Control.Monad.Trans.Except as Except
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as Aeson
import qualified Data.Char as Char
import Data.Functor.Identity (Identity)
import qualified Data.Functor.Identity as Identity
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
type Parse a
    = ExceptT ParserError
        (Parsec.ParsecT String FileContent (Indent.IndentT Identity))
        a


type Parser a
    = Parsec.ParsecT String FileContent (Indent.IndentT Identity) a


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


fail :: ParserError -> Parse a
fail =
    Except.throwE


fileContent :: Parse FileContent
fileContent =
    lift Parsec.getState


toParserError :: Parsec.ParseError -> [ParserError]
toParserError error =
    ParserError.errorMessages error
        |> map ParserError.messageString
        |> map encodeUtf8
        |> map Aeson.decode
        |> map (Maybe.withDefault (RawError <| show error))


runParser :: Parse a -> FilePath -> FileContent -> Either [ParserError] a
runParser parser filePath fileContent =
    let
        -- To allow differentiating last file char from position bumped
        -- too far after last lexeme parsing
        withNewLine =
            fileContent ++ "\n"
    in
    Except.runExceptT parser
        |> \p -> Indent.runIndentParserT p withNewLine filePath withNewLine
        |> Identity.runIdentity
        |> Either.mapLeft toParserError
        |> bind (Either.mapLeft List.singleton)


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


constructorIdentifier :: Parse ConstructorId
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


dataIdentifier :: Parse DataId
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



referenceIdentifier :: Parse ReferenceId
referenceIdentifier = do
    identifier
        |> map Identifier.referenceId


typeIdentifier :: Parse TypeId
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


typeVariableIdentifier :: Parse TypeVariableId
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


identifier :: Parse String
identifier =
    Token.identifier lexer
        |> lift


reserved :: String -> Parse ()
reserved =
    Token.reserved lexer
        >> lift


reservedOperator :: String -> Parse ()
reservedOperator =
    Token.reservedOp lexer
        >> lift


charLiteral :: Parse Char
charLiteral =
    Token.charLiteral lexer
        |> lift


numberLiteral :: Parse (Either Integer Double)
numberLiteral =
    Token.naturalOrFloat lexer
        |> lift


stringLiteral :: Parse String
stringLiteral =
    Token.stringLiteral lexer
        |> lift


mapParser :: (Parser a -> Parser b) -> Parse a -> Parse b
mapParser f =
    Except.mapExceptT (sequence >> map f >> sequence)


mapParser2
    :: (Parser a -> Parser b -> Parser c) -> Parse a -> Parse b -> Parse c
mapParser2 f a b =
    let
        parserA = Except.runExceptT a |> sequence
        parserB = Except.runExceptT b |> sequence
    in
    map2 f parserA parserB
        |> sequence
        |> ExceptT


mapParserList :: ([Parser a] -> Parser a) -> [Parse a] -> Parse a
mapParserList f ps =
    let
        parsers =
            traverse (Except.runExceptT >> sequence) ps
    in
    parsers
        |> map f
        |> sequence
        |> ExceptT


maybe :: Parse a -> Parse (Maybe a)
maybe =
    mapParser (Parsec.try >> Parsec.optionMaybe)


many :: Parse a -> Parse [a]
many =
    mapParser Parsec.many


manyUntil :: Parse a -> Parse b -> Parse [b]
manyUntil end p =
    mapParser2
        Parsec.manyTill
        p
        end


atLeastOne :: Parse a -> Parse (NonEmpty a)
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
        |> mapParser


oneOf :: [Parse a] -> Parse a
oneOf =
    mapParserList Parsec.choice


between :: Parse () -> Parse () -> Parse c -> Parse c
between before after mainParser = do
    before
    x <- mainParser
    after
    return x


unconsumeOnFailure :: Parse a -> Parse a
unconsumeOnFailure =
    mapParser Parsec.try


unconsumeOnFailure_ :: Parse a -> Parse a
unconsumeOnFailure_ =
    mapParser Parsec.try


catchRaw :: ParserError -> Parse a -> Parse a
catchRaw error parser = do
    result <- mapParser Parsec.optionMaybe parser
    case result of
        Just x ->
            return x
        Nothing ->
            fail error


endOfFile :: Parse ()
endOfFile =
    lift Parsec.eof


-- Position


position :: Parse Position
position =
    map
        (\sourcePosition ->
            Position
                (Parsec.sourceName sourcePosition)
                (Parsec.sourceLine sourcePosition)
                (Parsec.sourceColumn sourcePosition)
        )
        Parsec.getPosition
        |> lift


endPosition :: Parse Position
endPosition = do
    sourceCode <- fileContent
    Position filename currentLine currentColumn <- position

    let beforePosition = cutFrom currentLine currentColumn sourceCode
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

topLevel :: Parse ()
topLevel =
    lift Indent.topLevel


withPositionReference :: Parse a -> Parse a
withPositionReference =
    mapParser Indent.withPos


indented :: Parse ()
indented =
    lift Indent.indented


sameLine :: Parse ()
sameLine =
    lift Indent.same


sameLineOrIndented :: Parse ()
sameLineOrIndented =
    lift Indent.sameOrIndented
