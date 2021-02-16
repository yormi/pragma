module Parser.Parser
    ( Parser
    , SourceCode
    , bug
    , catch
    , consumeChar
    , consumeString
    , debug
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

    , until
    )
    where

import qualified Control.Applicative as Applicative
import Control.Monad.Trans.State (StateT)
import qualified Control.Monad.Trans.State as State
import Control.Monad.Trans.Except (Except)
import qualified Control.Monad.Trans.Except as Except

import Parser.Model.Error (Error)
import qualified Parser.Model.Error as Error
import Parser.Model.Position (Position(..))
import Parser.Model.Quote (Quote)
import qualified Parser.Model.Quote as Quote
import qualified Utils.List as List
import qualified Utils.String as String
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


run :: FilePath -> SourceCode -> Parser a -> Either Error a
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


fail :: Error -> Parser a
fail = do
    Except.throwE >> lift


bug :: String -> Parser a
bug explanation = do
    from <- getPosition
    Error.ThisIsABug from explanation
        |> fail



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



-- STATE


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
                    bug "Desired string can't be empty"
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



-- DEBUG


debug :: String -> Parser ()
debug str = do
    printInput str
    printPosition str
    -- printRefPosition str
    printNewLine
    trace "---" (return ())


print :: String -> String -> String -> Parser ()
print what section content =
    trace
        (what ++ " - " ++ section ++ ": " ++ content)
        (return ())


printInput :: String -> Parser ()
printInput str = do
    remaining <- getRemaining
    print "INPUT" str <| List.take 6 remaining


printPosition :: String -> Parser ()
printPosition str = do
    p <- getPosition
    print "POS" str <| show p


-- printRefPosition :: String -> Parser ()
-- printRefPosition str = do
--     a <-
--         Reader.ask
--             |> map Right
--             |> Parser
--     print "REF" str <| show a


printNewLine :: Parser ()
printNewLine =
    trace "\n" <| return ()



-- SUB-PARSE


until :: Parser a -> Parser b -> Parser b
until stopper parser =
    let
        setRemaining str =
            State.modify (\state -> state { remainingSourceCode = str })
    in do
        from <- getPosition
        initialRemaining <- getRemaining
        lengthToParse <- lengthBeforeStopper stopper
        let toParse = List.take lengthToParse initialRemaining
        let after = List.drop lengthToParse initialRemaining

        setRemaining toParse
        result <- parser
        hasConsumedEverything <- getRemaining |> map String.isEmpty

        if hasConsumedEverything then do
            setRemaining after
            return result

        else
            fail <| Error.ThisIsABug from "TODO"


lengthBeforeStopper :: Parser a -> Parser Int
lengthBeforeStopper parser =
    let
        recursive initialRemaining = do
            current <- getRemaining
            parser
                |> map
                    (\_ ->
                        String.length initialRemaining - String.length current
                    )
                |> recoverParser
                    (case current of
                        c : _ -> do
                            _ <- consumeChar c
                            recursive initialRemaining

                        [] ->
                            return <| String.length initialRemaining
                    )
    in do
    initialRemaining <- getRemaining
    recursive initialRemaining
        |> lookAhead
