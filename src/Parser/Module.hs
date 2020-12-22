module Parser.Module
    ( moduleParser
    ) where

import Control.Monad as Monad

import qualified AST.CodeQuote as CodeQuote
import qualified AST.Expression as E
import AST.Module (Module(..), TopLevel(..))
import qualified AST.TypeAnnotation as T
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
        Parser.maybe <| typeLineParser

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


typeLineParser :: Parser (E.Identifier, T.TypeAnnotation)
typeLineParser = do
    functionName <- Parser.identifier
    Parser.reservedOperator ":"
    type_ <- typeAnnotationParser
    return (functionName, type_)


typeAnnotationParser :: Parser T.TypeAnnotation
typeAnnotationParser =
    let
        functionTypeParser =
            Parser.unconsumeOnFailure <|
                do -- careful not to start with typeParser.
                   -- It creates an infinite loop =/
                    arg <- simpleTypeParser
                    Parser.reservedOperator "->"
                    returnType <- typeAnnotationParser
                    return <| T.Function arg returnType
    in
    Parser.oneOf [ functionTypeParser, simpleTypeParser ]


simpleTypeParser :: Parser T.TypeAnnotation
simpleTypeParser =
    Parser.oneOf
        [ map (const T.Bool) <| Parser.reserved "Bool"
        , map (const T.Int) <| Parser.reserved "Int"
        , map (const T.Float) <| Parser.reserved "Float"
        , map (const T.Char) <| Parser.reserved "Char"
        , map (const T.String) <| Parser.reserved "String"
        , do
            Parser.reservedOperator "("
            t <- typeAnnotationParser
            Parser.reservedOperator ")"
            return t
        , map (T.Variable) <| Parser.identifier
        ]


