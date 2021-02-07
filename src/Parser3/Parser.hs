module Parser3.Parser
    ( Parser
    , consumeChar
    , consumeString
    , fail
    , getPosition
    , getRemaining
    , lookAhead
    , recoverParser
    , run
    , unconsumeOnFailure
    )
    where

import Control.Monad.Trans.State (StateT)
import qualified Control.Monad.Trans.State as State
import Control.Monad.Trans.Except (Except)
import qualified Control.Monad.Trans.Except as Except

import Parser3.Error (Error)
import qualified Parser3.Error as E
import Parser3.Position (Column, Position(..))
import qualified Parser3.Position as Position
import Parser3.Quote (Quote)
import qualified Parser3.Quote as Quote
import qualified Utils.List as List


type Parser a
    = StateT State (Except Error) a


type SourceCode =
    String


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


unconsumeOnFailure :: Parser a -> Parser a
unconsumeOnFailure p =
    fromExcept <|
        \state ->
            let
                except =
                    toExcept state p
            in
            Except.catchE except (fail >> toExcept state)


recoverParser :: Parser a -> Parser a -> Parser a
recoverParser recoveringParser p =
    fromExcept <|
        \state ->
            toExcept state p
                |> recoverExcept (toExcept state recoveringParser)


recoverExcept :: Except a b -> Except a b -> Except a b
recoverExcept recoveringExcept except =
    Except.catchE except (const recoveringExcept)


lookAhead :: Parser a -> Parser a
lookAhead parser = do
    previousState <- State.get
    result <- parser
    State.put previousState
    return result


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
            Position.toColumn initialPosition

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
    Quote.fromPositions from to
        |> return
