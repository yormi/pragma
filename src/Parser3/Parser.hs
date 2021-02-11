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
    , mapError
    , moreRelevant
    , recoverParser
    , run
    , setReferencePosition
    , unconsumeOnFailure
    )
    where

import Control.Monad.Trans.State (StateT)
import qualified Control.Monad.Trans.State as State
import Control.Monad.Trans.Except (Except)
import qualified Control.Monad.Trans.Except as Except

import Parser3.Model.Error (Error)
import qualified Parser3.Model.Error as E
import Parser3.Model.Position (Position(..))
import Parser3.Model.Quote (Quote)
import qualified Parser3.Model.Quote as Quote
import qualified Utils.Either as Either
import qualified Utils.List as List
import qualified Utils.String as String
import qualified Utils.Tuple as Tuple


type Parser a
    = StateT State (Except ErrorRank) a


type SourceCode =
    String


data State
    = State
        { remainingSourceCode :: SourceCode
        , currentPosition :: Position
        , referencePosition :: Position
        }
        deriving (Eq, Show)


data ErrorRank =
    ErrorRank
        { metric :: ErrorMetric
        , error :: Error
        }
        deriving (Eq, Show)


type ErrorMetric =
    Int


fail :: Error -> Parser a
fail error = do
    metric <- errorMetric
    ErrorRank metric error
        |> Except.throwE
        |> lift



-- CATCH


catch :: Parser a -> Parser (Either ErrorRank a)
catch parser =
    fromExcept <|
        \state ->
            let
                except =
                    toExcept state parser
                        |> map (Tuple.mapFirst Right)
            in do
            Except.catchE except (\e -> return (Left e, state))


errorMetric :: Parser ErrorMetric
errorMetric = do
    remaining <- getRemaining
    return <| String.length remaining


moreRelevant :: ErrorRank -> ErrorRank -> Error
moreRelevant e1 e2 =
    if metric e1 < metric e2 then
        error e1

    else
        error e2



-- EXCEPT


fromExcept :: (State -> Except ErrorRank (a, State)) -> Parser a
fromExcept =
    State.StateT


toExcept :: State -> Parser a -> Except ErrorRank (a, State)
toExcept state parser =
    State.runStateT parser state


--


mapError :: (Error -> Error) -> Parser a -> Parser a
mapError f parser =
    fromExcept <|
        \state ->
            toExcept state parser
                |> Except.withExcept
                    (\(ErrorRank metric error) -> ErrorRank metric <| f error)


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
        |> Either.mapLeft error


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
