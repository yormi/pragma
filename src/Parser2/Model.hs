module Parser2.Model
    ( Error(..)
    , Parsed(..)
    , Parser
    , Quote(..)
    , RawError(..)
    , run

    -- COMBINATORS
    , anyChar
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


type SourceCode
    = String


data Error
    = RawError RawError
    | CustomError Error.ParserError
    deriving (Eq, Show)


data RawError
    = EndOfFileReached
    | StringExpected Position
    | SpaceExpected Position Char
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
        deriving (Eq, Show)


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


consume :: Char -> Parser ()
consume c = do
    updateRemaining
    updatePosition c


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
    from <- getPosition
    remaining <- getRemaining
    case remaining of
        c : _ -> do
            consume c -- Returns the Quote !??!?!?
            return (Parsed (quoteCode from from) c)

        _ ->
            basicFailure EndOfFileReached


space :: QuotedParser ()
space = do
    from <- getPosition
    Parsed quote c <- anyChar
    if c == ' ' then
        return <| Parsed quote ()

    else
        basicFailure <| SpaceExpected from c



string :: QuotedParser String
string = do
    remaining <- getRemaining
    let firstWord = List.takeWhile (not << Char.isSpace) remaining

    from <- getPosition
    case firstWord of
        [] ->
            basicFailure <| StringExpected from

        str -> do
            traverse consume str
            to <- getPosition
            Parsed (quoteCode from to) str
                |> return


