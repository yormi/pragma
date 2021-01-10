module Parser.Value (valueParser) where

import Data.Bifunctor (bimap)

import AST.Expression (Value(..), BoolLiteral(..))
import Parser.Model (Parser)
import qualified Parser.Parser as Parser
import qualified Utils.Either as Either


valueParser :: Parser Value
valueParser =
    Parser.oneOf
        [ map Bool boolParser
        , map Char Parser.charLiteral
        , numberParser
        , map String Parser.stringLiteral
        ]


numberParser :: Parser Value
numberParser =
    Parser.oneOf
        [ do
            Parser.reservedOperator "-"
            n <- Parser.numberLiteral
            n
                |> bimap ((*) (-1)) ((*) (-1))
                |> Either.fold Int Float
                |> return

        , Parser.numberLiteral
            |> map (Either.fold Int Float)
        ]


boolParser :: Parser BoolLiteral
boolParser =
    Parser.oneOf
        [ map (const TrueLiteral) <| Parser.reserved "True"
        , map (const FalseLiteral) <| Parser.reserved "False"
        ]
