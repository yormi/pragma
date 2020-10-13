module Parser (Parser) where

import qualified Text.Parsec.Indent as Indent


type Parser a = Indent.IndentParser String () a
