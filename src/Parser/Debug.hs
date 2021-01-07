module Parser.Debug (printAll) where

import qualified Control.Monad.Reader as Reader
import qualified Data.List as List
import qualified Text.Parsec as Parser

import Parser.Parser (Parser)


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
    i <- Parser.getInput
    print "INPUT" str <| List.take 6 (show i)


printPosition :: String -> Parser ()
printPosition str = do
    p <- Parser.getPosition
    print "POS" str <| show p


printRefPosition :: String -> Parser ()
printRefPosition str = do
    a <- Reader.ask
    print "REF" str <| show a


printNewLine :: Parser ()
printNewLine =
    trace "\n" <| return ()
