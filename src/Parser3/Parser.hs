module Parser3.Parser
    ( Parser
    , catch
    , consumeChar
    , consumeString
    , fail
    , getPosition
    , getReferencePosition
    , getRemaining
    , lookAhead
    , map3
    , mapError
    , recoverParser
    , run
    , setReferencePosition
    , unconsumeOnFailure
    )
    where

import qualified Control.Applicative as Applicative
import Control.Monad.Trans.State (StateT)
import qualified Control.Monad.Trans.State as State
import Control.Monad.Trans.Except (Except)
import qualified Control.Monad.Trans.Except as Except

import Parser3.Model.Error (Error)
import qualified Parser3.Model.Error as E
import Parser3.Model.Position (Position(..))
import Parser3.Model.Quote (Quote)
import qualified Parser3.Model.Quote as Quote
import qualified Utils.List as List
import qualified Utils.Tuple as Tuple


type Parser a
    = StateT State (Except Error) a


type SourceCode =
    String


data State
    = State
        { remainingSourceCode :: SourceCode
        , currentPosition :: Position
        , referencePosition :: Position
        }
        deriving (Eq, Show)


fail :: Error -> Parser a
fail = do
    Except.throwE >> lift



-- CATCH


catch :: Parser a -> Parser (Either Error a)
catch parser =
    fromExcept <|
        \state ->
            let
                except =
                    toExcept state parser
                        |> map (Tuple.mapFirst Right)
            in do
            Except.catchE except (\e -> return (Left e, state))



-- EXCEPT


fromExcept :: (State -> Except Error (a, State)) -> Parser a
fromExcept =
    State.StateT


toExcept :: State -> Parser a -> Except Error (a, State)
toExcept state parser =
    State.runStateT parser state


--


unconsumeOnFailure :: Parser a -> Parser a
unconsumeOnFailure p =
    fromExcept <|
        \state ->
            let
                except =
                    toExcept state p
            in
            Except.catchE except Except.throwE


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

        initialState =
            State sourceCode initialPosition initialPosition
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


getReferencePosition :: Parser Position
getReferencePosition =
    State.get
        |> map referencePosition


setReferencePosition :: Position -> Parser ()
setReferencePosition position =
    State.modify (\state -> state { referencePosition = position })



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



-- MAP


map3 :: Applicative f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
map3 =
    Applicative.liftA3


mapError :: (Error -> Error) -> Parser a -> Parser a
mapError f parser =
    fromExcept <|
        \state ->
            toExcept state parser
                |> Except.withExcept f
