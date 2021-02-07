module Parser3.Lexeme
    ( identifier
    , parenthesized
    , reserved
    , operator
    ) where

import qualified Parser3.Combinator as C
import Parser3.Error (Error(..))
import Parser3.Position (Position(..))
import qualified Parser3.Quote as Quote
import Parser3.Quote (Quote(..))
import Parser3.Parser (Parser)
import qualified Parser3.Parser as Parser
import qualified Utils.List as List
import qualified Utils.String as String
import qualified Utils.Tuple as Tuple


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
    [ "\\", "->"
    , "_"
    , ">>", "<<", "|>", "<|"
    , "{", "}", ":", "=", ","
    , "[", "]"
    , "==", "/=", ">=", "<=", ">", "<"
    , "+", "-", "*", "/", "//"
    ]


operator :: String -> Parser ()
operator desiredString = do
    C.someSpace
    from <- Parser.getPosition

    if String.isEmpty desiredString then
        "The required operator should not be empty"
            |> ThisIsABug from
            |> Parser.fail
    else
        Parser.unconsumeOnFailure <| do
            parsedChars <-
                List.range 0 (List.length desiredString)
                    |> traverse (const C.anyChar)

            let comparingString =
                    map Tuple.second parsedChars

            if not <| isAnOperator desiredString then
                desiredString ++ " is not an operator"
                    |> ThisIsABug from
                    |> Parser.fail

            else if comparingString /= desiredString then
                Parser.fail <| OperatorExpected from desiredString

            else
                return ()


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
