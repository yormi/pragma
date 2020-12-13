module Parser.Module
    ( moduleParser
    ) where

import Control.Monad as Monad

import qualified AST.CodeQuote as CodeQuote
import AST.Module (Module(..), TopLevel(..))
import qualified Type as T
import Parser.Parser (Parser)
import qualified Parser.Expression as Expression
import qualified Parser.Parser as Parser


moduleParser :: Parser Module
moduleParser =
    Parser.oneOf
        [ function
        ]
        |> Parser.many
        |> map Module


function :: Parser TopLevel
function = do
    from <- Parser.position
    Parser.topLevel

    typeLine <-
        Parser.maybe <| do
            functionName <- Parser.identifier
            Parser.reservedOperator ":"
            type_ <- typeParser
            return (functionName, type_)

    functionName <- Parser.identifier
    params <- Parser.many Parser.identifier
    Parser.reservedOperator "="

    body <- Parser.withPositionReference Expression.expressionParser
    to <- Parser.position

    let codeQuote = CodeQuote.fromPositions from to

    case typeLine of
        Just (typeLineName, type_) ->
            if typeLineName == functionName then
                return <| Function codeQuote type_ functionName params body
            else
                Monad.fail <|
                    "The function signature for '"
                        ++ typeLineName
                        ++ "' needs a body"

        Nothing ->
            Monad.fail <|
                "The function '" ++ functionName ++ "' needs a signature"



typeParser :: Parser T.Type
typeParser =
    let
        functionTypeParser =
            Parser.unconsumeOnFailure <|
                do -- careful not to start with typeParser.
                   -- It creates an infinite loop =/
                    a <- simpleTypeParser
                    Parser.reservedOperator "->"
                    b <- typeParser
                    return <| T.Function (T.FunctionType a b)
    in
    Parser.oneOf [ functionTypeParser, simpleTypeParser ]


simpleTypeParser :: Parser T.Type
simpleTypeParser =
    Parser.oneOf
        [ map (const T.Bool) <| Parser.reserved "Bool"
        , map (const T.Int) <| Parser.reserved "Int"
        , map (const T.Float) <| Parser.reserved "Float"
        , map (const T.Char) <| Parser.reserved "Char"
        , map (const T.String) <| Parser.reserved "String"
        , do
            Parser.reservedOperator "("
            t <- typeParser
            Parser.reservedOperator ")"
            return t
        ]


