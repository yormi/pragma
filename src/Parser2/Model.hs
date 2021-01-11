module Parser2.Model
    ( Error(..)
    , Parsed(..)
    , Parser
    , Position(..)
    , Quote(..)
    , RawError(..)
    , run

    -- COMBINATORS
    , anyChar
    , identifier
    , space
    , string
    )
    where

import Control.Monad.Trans.State (StateT)
import qualified Control.Monad.Trans.State as State
import Control.Monad.Trans.Except (Except)
import qualified Control.Monad.Trans.Except as Except
import qualified Data.Char as Char
import qualified GHC.Show

import qualified Parser.Error as Error
import qualified Utils.List as List
import qualified Utils.Tuple as Tuple


type Parser a
    = StateT State (Except Error) a


type QuotedParser a
    = Parser (Parsed a)


data Parsed a =
    Parsed
        { quote :: Quote
        , result :: a
        }
        deriving (Eq, Show)


resultOnly :: QuotedParser a -> Parser a
resultOnly =
    map result


quoteOnly :: QuotedParser a -> Parser Quote
quoteOnly =
    map quote


--- QUOTE ---


data Quote =
    Quote
        { filePath :: String
        , fromLine :: Int
        , fromColumn :: Int
        , toLine :: Int
        , toColumn :: Int
        }
        deriving (Eq)


instance Show Quote where
    show q =
        show (fromLine q) ++ ":" ++ show (fromColumn q)
            ++ "-"
            ++ show (toLine q) ++ ":" ++ show (toColumn q)


quoteCode :: Position -> Position -> Quote
quoteCode from to =
    Quote
        { filePath = filePath (from :: Position)
        , fromLine = line from
        , fromColumn = column from
        , toLine = line to
        , toColumn = column to
        }


fromPosition :: Quote -> Position
fromPosition q =
    Position
        { filePath = filePath (q :: Quote)
        , line = fromLine q
        , column = fromColumn q
        }


---


type SourceCode
    = String


data Error
    = RawError RawError
    | CustomError Error.ParserError
    deriving (Eq, Show)


data RawError
    = EndOfFileReached
    | StringExpected Position
    | SpaceExpected Quote Char
    | InvalidCharactersInIdentifier [ (Position, Char) ]
    deriving (Eq, Show)


data State
    = State
        { remainingSourceCode :: SourceCode
        , currentPosition :: Position
        }
        deriving (Eq, Show)


data Position
    = Position
        { filePath :: String
        , line :: Int
        , column :: Int
        }
        deriving (Eq)


instance Show Position where
    show (Position _ line column) =
        show line ++ ":" ++ show column


---


basicFailure :: RawError -> Parser a
basicFailure =
    RawError
        >> Except.throwE
        >> lift


run :: String -> SourceCode -> Parser a -> Either Error a
run filePath sourceCode parser =
    let
        initialPosition =
            Position
                { filePath = filePath
                , line = 1
                , column = 1
                }

        initialState =
            State sourceCode initialPosition
    in
    State.evalStateT parser initialState
        |> Except.runExcept


getRemaining :: Parser SourceCode
getRemaining =
    State.get
        |> map remainingSourceCode


getPosition :: Parser Position
getPosition =
    State.get
        |> map currentPosition


consumeChar :: Char -> Parser Position
consumeChar c = do
    position <- getPosition
    updateRemaining
    updatePosition c
    return position


consumeString :: String -> Parser Quote
consumeString str =
    let
        consume remainingString =
            case remainingString of
                c : [] -> do
                    to <- getPosition
                    consumeChar c
                    return to


                c : rest -> do
                    consumeChar c
                    consume rest
    in do
    from <- getPosition
    to <- consume str
    quoteCode from to
        |> return


updateRemaining :: Parser ()
updateRemaining =
    State.modify
        (\state ->
            remainingSourceCode state
                |> List.drop 1
                |> (\r -> state { remainingSourceCode = r })
        )


updatePosition :: Char -> Parser ()
updatePosition c =
    State.modify (\state ->
        let
            Position { filePath, line, column } =
                currentPosition state

            tabWidth =
                4

            newPosition =
                case c of
                    '\n' ->
                        Position filePath (line + 1) 1
                    '\t' ->
                        Position
                            filePath
                            line
                            (column + tabWidth - ((column-1) `mod` tabWidth))
                    _ ->
                        Position filePath line (column + 1)
        in
        state { currentPosition = newPosition }
    )


--- COMBINATORS ---

anyChar :: QuotedParser Char
anyChar = do
    remaining <- getRemaining
    case remaining of
        c : _ -> do
            position <- consumeChar c
            return <| Parsed (quoteCode position position) c

        _ ->
            basicFailure EndOfFileReached


space :: QuotedParser ()
space = do
    Parsed quote c <- anyChar
    if c == ' ' then
        return <| Parsed quote ()

    else
        basicFailure <| SpaceExpected quote c


string :: QuotedParser String
string = do
    remaining <- getRemaining
    let firstWord = List.takeWhile (not << Char.isSpace) remaining

    from <- getPosition
    case firstWord of
        [] ->
            basicFailure <| StringExpected from

        str -> do
            quote <- consumeString str
            Parsed quote str
                |> return


--- TOKEN ---

identifierCharacters :: String
identifierCharacters =
    "abcdefghijklmnopqrstuvwxyz"
    ++ "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    ++ "1234567890"


identifier :: QuotedParser String
identifier = do
    (Parsed quote str) <- string
    let invalidCharacters =
            str
                |> List.indexedMap (\index c -> (index, c))
                |> List.filter
                    (\(_, c) -> not <| List.contains c identifierCharacters)

    if List.isEmpty invalidCharacters then
        return <| Parsed quote str
    else
        invalidCharacters
            |> map
                (\(index, char) ->
                    quote
                        |> fromPosition
                        |> \p -> p { column = column p + index }
                        |> \position -> (position, char)
                )
            |> InvalidCharactersInIdentifier
            |> basicFailure
