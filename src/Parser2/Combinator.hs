module Parser2.Combinator
    ( anyChar
    , identifier
    , many
    , maybe
    , oneOf
    , operator
    , reserved
    , space
    , string
    )
    where

import qualified Data.Char as Char

import Parser2.Error (Error(..))
import Parser2.Model
    ( Parsed(..)
    , Position(..)
    , Quote(..)
    , fromPosition
    , quoteCode
    )
import Parser2.Parser (Parser, QuotedParser)
import qualified Parser2.Parser as Parser
import qualified Utils.List as List


anyChar :: QuotedParser Char
anyChar = do
    remaining <- Parser.getRemaining
    case remaining of
        c : _ -> do
            position <- Parser.consumeChar c
            return <| Parsed (quoteCode position position) c

        _ ->
            Parser.fail EndOfFileReached


space :: QuotedParser ()
space = do
    Parsed quote c <- anyChar
    if c == ' ' then
        return <| Parsed quote ()

    else
        Parser.fail <| SpaceExpected quote c


string :: QuotedParser String
string = do
    remaining <- Parser.getRemaining
    let firstWord = List.takeWhile (not << Char.isSpace) remaining

    from <- Parser.getPosition
    case firstWord of
        [] ->
            Parser.fail <| StringExpected from

        str -> do
            quote <- Parser.consumeString str
            return <| Parsed quote str


--- IDENTIFIER ---

identifierCharacters :: String
identifierCharacters =
    "abcdefghijklmnopqrstuvwxyz"
    ++ "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    ++ "1234567890"
    ++ "_"


identifier :: QuotedParser String
identifier = do
    (Parsed quote str) <- string

    if isAReservedWord str then
        IdentifierCantBeAReservedWord quote str
            |> Parser.fail

    else if List.isEmpty (invalidCharacters str) then
        return <| Parsed quote str

    else
        str
            |> invalidCharacters
            |> map
                (\(index, char) ->
                    quote
                        |> fromPosition
                        |> \p -> p { column = column p + index }
                        |> \position -> (position, char)
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
    ]


isAReservedWord :: String -> Bool
isAReservedWord str =
    List.contains str reservedWords


reserved :: String -> Parser Quote
reserved desiredString = do
    position <- Parser.getPosition
    Parsed quote parsedString <- string

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


operator :: String -> Parser Quote
operator desiredString = do
    position <- Parser.getPosition
    Parsed quote parsedString <- string

    if not <| isAnOperator desiredString then
        desiredString ++ " is not an operator"
            |> ThisIsABug position
            |> Parser.fail

    else if parsedString /= desiredString then
        Parser.fail <| OperatorExpected quote desiredString

    else
        return quote


isAnOperator :: String -> Bool
isAnOperator str =
    List.contains str operators


--- COMBINATORS ---


oneOf :: Error -> [Parser a] -> Parser a
oneOf error =
    let
        recursive remainingParsers =
            case remainingParsers of
                [] ->
                    Parser.fail error

                p : rest ->
                    Parser.recoverParser (recursive rest) p

    in
    recursive


many :: Parser a -> Parser [a]
many p =
    let
        recursive accumulated =
            (do
                parsed <- p
                recursive (accumulated ++ [parsed])
            )
                |> Parser.recoverParser (return accumulated)
    in
    recursive []


maybe :: Parser a -> Parser (Maybe a)
maybe p =
    p
        |> map Just
        |> Parser.recoverParser (return Nothing)
