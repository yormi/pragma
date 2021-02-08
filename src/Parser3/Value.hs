module Parser3.Value (parser) where

import AST3.Expression (Value(..), BoolLiteral(..))
import qualified AST3.Expression as Expression
import qualified Parser3.Combinator as C
import qualified Parser3.Error as Error
import qualified Parser3.Lexeme as Lexeme
import Parser3.Parser (Parser)
import qualified Parser3.Parser as Parser
import qualified Parser3.Quote as Quote
import qualified Utils.Maybe as Maybe
import qualified Utils.String as String
import qualified Utils.Tuple as Tuple


parser :: Parser Value
parser = do
    C.someSpace
    position <- Parser.getPosition
    C.oneOf
        (Error.ValueExpected position)
        [ boolParser
        , charLiteral
        , intParser
        , stringLiteral
        ]


charLiteral :: Parser Value
charLiteral = do
    from <- C.char '\''
    c <-
        C.oneOf
            (Error.CharExpected from)
            [ escapedNewLine
            , escapedTab
            , C.anyChar
                |> map Tuple.second
            ]
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
        (Error.BooleanExpected position)
        [ map TrueLiteral (Lexeme.reserved "True")
        , map FalseLiteral (Lexeme.reserved "False")
        ]
        |> map Bool


stringLiteral :: Parser Value
stringLiteral = do
    C.someSpace
    from <- C.char '"'
    str <-
        C.oneOf
            Error.EndOfFileReached
            [ escapedDoubleQuote
            , escapedNewLine
            , escapedTab
            , C.anyChar
                |> map Tuple.second
            ]
            |> C.until (C.char '"')
    to <- C.char '"'
    let quote = Quote.fromPositions from to
    String quote str
        |> return


escapedDoubleQuote :: Parser Char
escapedDoubleQuote = do
    _ <- C.char '\\'
    _ <- C.char '"'
    return '"'


escapedNewLine :: Parser Char
escapedNewLine = do
    _ <- C.char '\\'
    _ <- C.char 'n'
    return '\n'


escapedTab :: Parser Char
escapedTab = do
    _ <- C.char '\\'
    _ <- C.char 't'
    return '\t'
