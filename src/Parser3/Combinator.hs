module Parser3.Combinator
    ( anyChar
    , atLeastOne
    , char
    , many
    , maybe
    , oneOf
    , oneOf_
    , someSpace
    , space
    , string
    , until
    )
    where

import qualified Data.Char as Char

import Parser3.Error (Error(..))
import Parser3.Position (Position(..))
import Parser3.Quote (Quote(..))
import Parser3.Parser (Parser)
import qualified Parser3.Parser as Parser
import qualified Utils.List as List
import Utils.NonEmpty (NonEmpty)
import qualified Utils.NonEmpty as NonEmpty


until :: Parser a -> Parser b -> Parser [b]
until stopWith parser =
    (do
        _ <- Parser.lookAhead stopWith
        return []
    )
        |> Parser.recoverParser
            (do
                xAgain <- parser
                xs <- until stopWith parser
                return (xAgain : xs)
            )



anyChar :: Parser (Position, Char)
anyChar = do
    remaining <- Parser.getRemaining
    case remaining of
        c : _ -> do
            position <- Parser.consumeChar c
            return (position, c)

        _ ->
            Parser.fail EndOfFileReached


char :: Char -> Parser Position
char desiredChar = do
    (position, c) <- anyChar
    if c == desiredChar then
        return position

    else
        Parser.fail <| NotTheDesiredChar position desiredChar


someSpace :: Parser ()
someSpace =
    many space
        |> void


space :: Parser ()
space = do
    (position, c) <- anyChar
    if Char.isSpace c then
        return ()

    else
        Parser.fail <| SpaceExpected position c


string :: Parser (Quote, String)
string = do
    remaining <- Parser.getRemaining
    let firstWord = List.takeWhile (not << isEndingWord) remaining

    from <- Parser.getPosition
    case firstWord of
        [] ->
            Parser.fail <| StringExpected from

        str -> do
            quote <- Parser.consumeString str
            return (quote, str)


isEndingWord :: Char -> Bool
isEndingWord c =
    Char.isSpace c
        || List.contains c [ ')', ']' ]


--- COMBINATORS ---


oneOf_ :: [Parser a] -> Parser a
oneOf_ =
    let
        recursive remainingParsers =
            case remainingParsers of
                [] ->
                    shouldNotHappen

                [p] ->
                    p

                p : rest -> do
                    either <- Parser.catch p
                    case either of
                        Right x ->
                            return x

                        Left errorRank1 ->
                            onFailure errorRank1 rest

        onFailure errorRank rest = do
            either <- Parser.catch <| recursive rest
            case either of
                Right x ->
                    return x

                Left errorRank2 ->
                    Parser.moreRelevant errorRank errorRank2
                        |> Parser.fail

        shouldNotHappen = do
            position <- Parser.getPosition
            ThisIsABug position
                "At least one parser must be provided to the oneOf combinator"
                |> Parser.fail
    in
    recursive

--


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


atLeastOne :: Parser a -> Parser (NonEmpty a)
atLeastOne p = do
    position <- Parser.getPosition
    xs <- many p
    case NonEmpty.fromList xs of
        Just nonEmpty ->
            return nonEmpty

        Nothing ->
            Parser.fail <| AtLeastOneExpected position



maybe :: Parser a -> Parser (Maybe a)
maybe p =
    p
        |> map Just
        |> Parser.recoverParser (return Nothing)
