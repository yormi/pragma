module Parser.Model
    ( InternalParser
    , Parser(..)
    , State(..)
    , catchUncaughtError
    , fail
    , getState
    , runParser
    , toParserError
    ) where


import Control.Applicative (liftA2)
import qualified Control.Monad as Monad
import qualified Data.Aeson as Aeson
import qualified Data.Functor.Identity as Identity
import qualified Text.Parsec as Parsec
import qualified Text.Parsec.Error as ParserError
import qualified Text.Parsec.Indent as Indent

import Parser.Error
import qualified Utils.Either as Either
import qualified Utils.Maybe as Maybe


newtype Parser a
    = Parser (InternalParser a)

type InternalParser a =
    Indent.IndentParser String State
       (Either ParserError a)


instance Functor Parser where
    fmap f (Parser p) =
        let
            mapEither =
                map f

            mapParserTransformer =
                map mapEither
        in
        p
            |> mapParserTransformer
            |> Parser


instance Applicative Parser where
    pure a =
        Parser (return <| Right a)

    liftA2 f (Parser a) (Parser b) =
        Parser <| liftA2 (liftA2 f) a b


instance Monad Parser where
    (>>=) (Parser p) f =
        p
            |> bind
                (\result ->
                    case result of
                        Right x ->
                            f x
                                |>
                                    (\(Parser resultingParser) ->
                                        resultingParser
                                    )

                        Left e ->
                            return <| Left e
                )
            |> Parser


data State
    = State
        { fileContent :: FileContent
        , lastError :: Maybe ParserError
        }


type FileContent
    = String


fail :: ParserError -> Parser a
fail =
    Left
        >> return
        >> Parser


getState :: Parser State
getState =
    Parsec.getState
        |> map Right
        |> Parser


catchUncaughtError :: ParserError -> Parser a -> Parser a
catchUncaughtError error (Parser p) =
    (do
        result <- Parsec.optionMaybe p
        case result of
            Just r ->
                return r

            Nothing -> do
                return <| Left error
    )
            |> Parser


toParserError :: Parsec.ParseError -> [ParserError]
toParserError error =
    ParserError.errorMessages error
        |> map ParserError.messageString
        |> map
            (\str ->
                encodeUtf8 str
                    |> Aeson.decode
                    |> Maybe.withDefault RawError
            )


runParser :: Parser a -> FilePath -> FileContent -> Either ParserError a
runParser (Parser parser) filePath fileContent =
    let
        initialState =
            State fileContent Nothing
    in
    Indent.runIndentParserT parser initialState filePath fileContent
        |> Identity.runIdentity
        |> Either.withDefault (const <| Left MismatchInProcess)
