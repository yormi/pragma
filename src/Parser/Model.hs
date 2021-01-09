module Parser.Model
    ( FieldError(..)
    , FileContent
    , Parser
    , ParserError(..)
    , RecordError(..)
    , State(..)
    , TypeAliasError(..)
    , fail
    , getState
    , runParser
    , toParserError
    ) where


import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as Aeson
import qualified Text.Parsec as Parsec
import qualified Text.Parsec.Error as ParserError
import qualified Text.Parsec.Indent as Indent

import AST.CodeQuote (CodeQuote, Position(..))
import AST.Identifier (DataId)
import qualified Utils.Either as Either
import qualified Utils.List as List
import qualified Utils.Maybe as Maybe


-- Using this complex form because type synonym can't be partially applied.
-- We need partial application for abstracting the `a` away
type Parser a
    = Indent.IndentParser String State a


data State
    = State
        { fileContent :: FileContent
        , error :: Maybe ParserError
        }


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
fail error = do
    Parsec.modifyState (\s -> s { error = Just error })

    error
        |> Aeson.encode
        |> decodeUtf8
        |> Parsec.parserFail


getState :: Parser State
getState =
    Parsec.getState


toParserError :: Parsec.ParseError -> [ParserError]
toParserError error =
    ParserError.errorMessages error
        |> map ParserError.messageString
        |> map encodeUtf8
        |> map Aeson.decode
        |> map (Maybe.withDefault (RawError <| show error))


runParser :: Parser a -> FilePath -> FileContent -> Either [ParserError] a
runParser parser filePath sourceCode =
    let
        -- To allow differentiating last file char from position bumped
        -- too far after last lexeme parsing
        withNewLine =
            sourceCode ++ "\n"

        initialState =
            State
                { fileContent = withNewLine
                , error = Nothing
                }

        parserWithError = do
            state <- getState
            let maybeError = error state
            result <- Parser.optionMaybe parser
            case result of
                Just ast ->
                    Right ast

                Nothing ->
                    Left 
            return (ast, maybeError)
    in
    Indent.runIndentParser parserWithError initialState filePath withNewLine
        |> Either.mapLeft List.singleton
        |> bind (Either.mapLeft toParserError)

