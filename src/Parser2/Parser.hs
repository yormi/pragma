module Parser2.Parser
    ( Parser
    , QuotedParser
    , consumeChar
    , consumeString
    , fail
    , getPosition
    , getRemaining
    , mapResult
    , recoverParser
    , run
    )
    where

import Control.Monad.Trans.State (StateT)
import qualified Control.Monad.Trans.State as State
import Control.Monad.Trans.Except (Except)
import qualified Control.Monad.Trans.Except as Except

import Parser2.Error (Error)
import qualified Parser2.Error as E
import Parser2.Model
    ( Column
    , Parsed(..)
    , Position(..)
    , Quote
    , SourceCode
    , positionToColumn
    , quoteCode
    )
import qualified Utils.List as List


type Parser a
    = StateT State (Except Error) a


type QuotedParser a
    = Parser (Parsed a)


data State
    = State
        { remainingSourceCode :: SourceCode
        , currentPosition :: Position
        , referenceIndentation :: Column
        }
        deriving (Eq, Show)


fail :: Error -> Parser a
fail =
    Except.throwE >> lift


fromExcept :: (State -> Except Error (a, State)) -> Parser a
fromExcept =
    State.StateT


toExcept :: State -> Parser a -> Except Error (a, State)
toExcept state parser =
    State.runStateT parser state


recoverParser :: Parser a -> Parser a -> Parser a
recoverParser recoveringParser p =
    fromExcept <|
        \state ->
            toExcept state p
                |> recoverExcept (toExcept state recoveringParser)


recoverExcept :: Except a b -> Except a b -> Except a b
recoverExcept recoveringExcept except =
    Except.catchE except (const recoveringExcept)


run :: String -> SourceCode -> Parser a -> Either Error a
run filePath sourceCode parser =
    let
        initialPosition =
            Position
                { filePath = filePath
                , line = 1
                , column = 1
                }

        initialColumn =
            positionToColumn initialPosition

        initialState =
            State sourceCode initialPosition initialColumn
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
                [c] -> do
                    to <- getPosition
                    _ <- consumeChar c
                    return to


                c : rest -> do
                    _ <- consumeChar c
                    consume rest

                [] -> do
                    position <- getPosition
                    E.ThisIsABug position "Desired string can't be empty"
                        |> fail
    in do
    from <- getPosition
    to <- consume str
    quoteCode from to
        |> return


mapResult :: (a -> b) -> QuotedParser a -> QuotedParser b
mapResult f parser = do
    Parsed quote result <- parser
    Parsed quote (f result)
        |> return
