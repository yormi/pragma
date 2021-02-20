module Parser.Lexeme
    ( identifier
    , parenthesized
    , reserved
    , operator
    ) where

import qualified Control.Monad as Monad

import qualified Parser.Combinator as C
import Parser.Model.Error (Error(..))
import Parser.Model.Position (Position(..))
import qualified Parser.Model.Quote as Quote
import Parser.Model.Quote (Quote(..))
import Parser.Parser (Parser)
import qualified Parser.Parser as Parser
import qualified Utils.List as List
import qualified Utils.NonEmpty as NonEmpty


-- *********************************************
-- Lexeme:
--     Swallowing spaces before & Contains Quote
-- *********************************************



--- IDENTIFIER ---


identifierCharacters :: String
identifierCharacters =
    "abcdefghijklmnopqrstuvwxyz"
    ++ "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    ++ "1234567890"
    ++ "_"


identifier :: Parser (Quote, String)
identifier = do
    C.someSpace
    (quote, str) <- C.string

    if isAReservedWord str then
        IdentifierCantBeAReservedWord quote str
            |> Parser.fail

    else if List.isEmpty (invalidCharacters str) then
        return (quote, str)

    else
        str
            |> invalidCharacters
            |> map
                (\(index, invalidChar) ->
                    quote
                        |> Quote.from
                        |> \p -> p { column = column p + index }
                        |> \position -> (position, invalidChar)
                )
            |> InvalidCharactersInIdentifier
            |> Parser.fail


invalidCharacters :: String -> [ (Int, Char) ]
invalidCharacters =
    List.indexedMap (\index c -> (index, c))
        >> List.filter
            (\(_, c) -> not <| List.contains c identifierCharacters)



--- RESERVED WORD ---


reservedWords :: [String]
reservedWords =
    [ "type", "alias"
    , "let", "in"
    , "if", "then", "else"
    , "case", "of"
    , "True", "False"
    , "Bool", "Char", "Int", "Float", "String"
    ]


isAReservedWord :: String -> Bool
isAReservedWord str =
    List.contains str reservedWords


reserved :: String -> Parser Quote
reserved desiredString = do
    C.someSpace
    position <- Parser.getPosition
    (quote, parsedString) <- C.string

    if not <| isAReservedWord desiredString then
        desiredString ++ " is not a reserved word"
            |> ThisIsABug position
            |> Parser.fail

    else if parsedString /= desiredString then
        Parser.fail <| ReservedWordExpected quote desiredString

    else
        return quote



--- OPERATOR ---


operators :: [String]
operators =
    [ "\\", "->" -- Lambda & Case & Type
    , "_" -- Wildcard
    , ">>", "<<", "|>", "<|" -- Pipe
    , "{", "}", ":", "=", "," -- Field
    , "[", "]" -- List
    , "==", "/=", ">=", "<=", ">", "<" -- Comparison
    , "+", "-", "*", "/", "//" -- Arithmetic
    , "|" -- Sum type
    ]


operator :: String -> Parser Quote
operator desiredString = do
    C.someSpace

    case desiredString of
        "" ->
            Parser.bug "The required operator should not be empty"

        c : rest ->
            if isAnOperator desiredString then do
                from <- Parser.getPosition
                NonEmpty.build c rest
                    |> Monad.mapM C.char
                    |> map
                        ( \positions ->
                            let
                                to =
                                    NonEmpty.last positions
                            in
                            Quote.fromPositions from to
                        )
                    |> Parser.mapError
                        (\_ -> OperatorExpected from desiredString)

            else
                Parser.bug <| desiredString ++ " is not an operator"


isAnOperator :: String -> Bool
isAnOperator str =
    List.contains str operators



--- PARENTHESIZED ---


parenthesized :: Parser a -> Parser a
parenthesized p = do
    C.someSpace
    _ <- C.char '('
    C.someSpace
    expr <- p
    C.someSpace
    _ <- C.char ')'
    return expr