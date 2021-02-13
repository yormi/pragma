module Parser3.Utils
    ( aFilePath
    , dataId
    , false
    , int
    , position
    , quote
    , reference
    , referenceId
    , run
    , true
    ) where

import qualified TestUtils

import AST3.Expression (Expression)
import qualified AST3.Expression as Expression
import qualified AST3.Identifier as Identifier
import Parser3.Model.Error (Error)
import Parser3.Model.Position (Position(..))
import Parser3.Model.Quote (Quote(..))
import Parser3.Parser (Parser, SourceCode)
import qualified Parser3.Parser as Parser
import qualified Utils.String as String


aFilePath :: FilePath
aFilePath =
    "some/File/Path.pa"


run :: SourceCode -> Parser a -> Either Error a
run sourceCode parser =
    Parser.run aFilePath sourceCode parser



-- POSITION & QUOTE


position :: Int -> Int -> Position
position line column =
    Position aFilePath line column


quote :: Int -> Int -> Int -> Int -> Quote
quote lineFrom columnFrom lineTo lineColumn =
    Quote aFilePath lineFrom columnFrom lineTo lineColumn


quoteFromLenght :: Int -> Int -> String -> Quote
quoteFromLenght fromLine fromColumn str =
    fromColumn + String.length str - 1
        |> Quote aFilePath fromLine fromColumn fromLine



-- IDENTIFIER


dataId :: Int -> Int -> String -> IO Identifier.DataId
dataId fromLine fromColumn str =
    let
        idQuote =
            quoteFromLenght fromLine fromColumn str
    in
    Identifier.dataId idQuote str
        |> TestUtils.assumeJust


referenceId :: Int -> Int -> String -> Identifier.ReferenceId
referenceId fromLine fromColumn str =
    let
        idQuote =
            quoteFromLenght fromLine fromColumn str
    in
    Identifier.referenceId idQuote "ff"


-- EXPRESSION

reference :: Int -> Int -> String -> Expression
reference fromLine fromColumn str =
    let
        referenceQuote =
            quoteFromLenght fromLine fromColumn str
    in
    Identifier.referenceId referenceQuote str
        |> Expression.Reference


int :: Int -> Int -> Int -> Expression
int fromLine fromColumn n =
    let
        numberQuote =
            quoteFromLenght fromLine fromColumn (show n)
    in
    Expression.Int numberQuote n
        |> Expression.Value


false :: Int -> Int -> Expression
false fromLine fromColumn =
    quoteFromLenght fromLine fromColumn "False"
        |> Expression.FalseLiteral
        |> Expression.Bool
        |> Expression.Value


true :: Int -> Int -> Expression
true fromLine fromColumn =
    quoteFromLenght fromLine fromColumn "True"
        |> Expression.TrueLiteral
        |> Expression.Bool
        |> Expression.Value
