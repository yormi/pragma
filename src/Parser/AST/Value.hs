module Parser.AST.Value (parser) where

import AST.Expression (Value(..), BoolLiteral(..))
import qualified AST.Expression as Expression
import qualified Parser.Combinator as C
import qualified Parser.Model.Error as Error
import Parser.Model.Position (Position)
import Parser.Model.Quote (Quote)
import qualified Parser.Model.Quote as Quote
import qualified Parser.Lexeme as Lexeme
import qualified Parser.Indentation as Indentation
import Parser.Parser (Parser)
import qualified Parser.Parser as Parser
import qualified Utils.List as List
import qualified Utils.Maybe as Maybe
import qualified Utils.String as String
import qualified Utils.Tuple as Tuple


parser :: Parser Value
parser = do
    C.someSpace
    C.oneOf
        [ boolParser
        , charLiteral
        , intParser
        , stringLiteral
        ]


charLiteral :: Parser Value
charLiteral = do
    from <- C.char '\''
    (_, c) <-
        C.oneOf
            [ escapedSingleQuote
            , escapedNewLine
            , escapedTab
            , C.anyCharBut [ '\'' ]
                |> map quotizeCharResult
            ]
            |> Parser.mapError (const <| Error.CharExpected from)
    to <- C.char '\''
    let quote = Quote.fromPositions from to
    Char quote c
        |> return


intParser :: Parser Value
intParser = do
    C.someSpace
    from <- Parser.getPosition

    minusSign <- C.maybe <| Lexeme.operator "-"
    (stringQuote, str) <- C.string

    let to = Quote.to stringQuote
    let quote = Quote.fromPositions from to

    case String.toInt str of
        Just n ->
            minusSign
                |> map (const <| -1 * n)
                |> Maybe.withDefault n
                |> Expression.Int quote
                |> return

        Nothing ->
            Parser.fail <| Error.IntExpected from


boolParser :: Parser Value
boolParser = do
    position <- Parser.getPosition
    C.oneOf
        [ map TrueLiteral (Lexeme.reserved "True")
        , map FalseLiteral (Lexeme.reserved "False")
        ]
        |> map Bool
        |> Parser.mapError (\_ -> Error.BooleanExpected position)


stringLiteral :: Parser Value
stringLiteral = do
    C.someSpace
    Indentation.withPositionReference <| do
        from <- C.char '"'
        (innerQuote, str) <- sameLineChars

        C.someSpace
        Indentation.sameLine
            |> Parser.mapError
                (\_ ->
                    let
                        errorQuote =
                            innerQuote
                                |> map Quote.to
                                |> Maybe.withDefault from
                                |> Quote.fromPositions from
                    in
                    Error.StringMustBeOnSingleLine errorQuote
                )

        to <- C.char '"'
        let quote = Quote.fromPositions from to
        return <| String quote str


sameLineChars :: Parser (Maybe Quote, String)
sameLineChars = do
    result <-
        (do
            Indentation.sameLine
            C.oneOf
                [ escapedDoubleQuote
                , escapedNewLine
                , escapedTab
                , C.anyCharBut [ '"' ]
                    |> map quotizeCharResult
                ]
        )
            |> C.many
    let quotes = map Tuple.first result
    let quote =
            Maybe.map2 Quote.fromPositions
                (map Quote.from <| List.head quotes)
                (map Quote.to <| List.last quotes)
    let str = map Tuple.second result
    return (quote, str)


quotizeCharResult :: (Position, Char) -> (Quote, Char)
quotizeCharResult (position, c) =
    let
        quote =
            Quote.fromPositions position position
    in
    (quote, c)


escapedSingleQuote :: Parser (Quote, Char)
escapedSingleQuote = do
    from <- C.char '\\'
    to <- C.char '\''
    let quote = Quote.fromPositions from to
    return (quote, '\'')


escapedDoubleQuote :: Parser (Quote, Char)
escapedDoubleQuote = do
    from <- C.char '\\'
    to <- C.char '"'
    let quote = Quote.fromPositions from to
    return (quote, '"')


escapedNewLine :: Parser (Quote, Char)
escapedNewLine = do
    from <- C.char '\\'
    to <- C.char 'n'
    let quote = Quote.fromPositions from to
    return (quote, '\n')


escapedTab :: Parser (Quote, Char)
escapedTab = do
    from <- C.char '\\'
    to <- C.char 't'
    let quote = Quote.fromPositions from to
    return (quote, '\t')
