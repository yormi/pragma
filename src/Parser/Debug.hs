module Parser.Debug (printAll) where

import qualified Control.Monad.Reader as Reader
import qualified Data.List as List
import qualified Text.Parsec as Parsec

import Parser.Model (Parser(..))


printAll :: String -> Parser ()
printAll str = do
    printInput str
    printPosition str
    printRefPosition str
    printNewLine


print :: String -> String -> String -> Parser ()
print what section content =
    trace
        (what ++ " - " ++ section ++ ": " ++ content)
        (return ())


printInput :: String -> Parser ()
printInput str = do
    i <-
        Parsec.getInput
            |> map Right
            |> Parser
    print "INPUT" str <| List.take 6 (show i)


printPosition :: String -> Parser ()
printPosition str = do
    p <-
        Parsec.getPosition
            |> map Right
            |> Parser
    print "POS" str <| show p


printRefPosition :: String -> Parser ()
printRefPosition str = do
    a <-
        Reader.ask
            |> map Right
            |> Parser
    print "REF" str <| show a


printNewLine :: Parser ()
printNewLine =
    trace "\n" <| return ()
