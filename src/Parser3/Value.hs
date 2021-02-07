module Parser3.Value (parser) where

import AST3.Expression (Value(..), BoolLiteral(..))
import qualified AST3.Expression as Expression
import qualified Parser3.Combinator as C
import qualified Parser3.Error as Error
import qualified Parser3.Lexeme as Lexeme
import Parser3.Parser (Parser)
import qualified Parser3.Parser as P
import qualified Parser3.Quote as Quote
import qualified Utils.Maybe as Maybe
import qualified Utils.String as String


parser :: Parser Value
parser = do
    C.someSpace
    position <- P.getPosition
    C.oneOf
        (Error.ValueExpected position)
        [ boolParser
        , charLiteral
        , intParser
        -- , map String Parser.stringLiteral
        ]


charLiteral :: Parser Value
charLiteral = do
    from <- C.char '\''
    (_, c) <- C.anyChar
    to <- C.char '\''
    let quote = Quote.fromPositions from to
    Char quote c
        |> return


intParser :: Parser Value
intParser = do
    C.someSpace
    from <- P.getPosition

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
            P.fail <| Error.IntExpected from


-- numberParser :: QuotedParser Value
-- numberParser =
--     C.oneOf
--         [ do
--             minusSign <- C.maybe <| C.operator "-"
--             C.many C.digit
-- 
--             n <- Parser.numberLiteral
--             n
--                 |> bimap ((*) (-1)) ((*) (-1))
--                 |> Either.fold Int Float
--                 |> return
-- 
--         , Parser.numberLiteral
--             |> map (Either.fold Int Float)
--         ]


boolParser :: Parser Value
boolParser = do
    position <- P.getPosition
    C.oneOf
        (Error.BooleanExpected position)
        [ map TrueLiteral (Lexeme.reserved "True")
        , map FalseLiteral (Lexeme.reserved "False")
        ]
        |> map Bool
