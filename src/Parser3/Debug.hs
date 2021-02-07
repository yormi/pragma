module Parser3.Debug (printAll) where

import qualified Data.List as List

import Parser3.Parser (Parser)
import qualified Parser3.Parser as Parser


printAll :: String -> Parser ()
printAll str = do
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
    remaining <- Parser.getRemaining
    print "INPUT" str <| List.take 6 remaining


printPosition :: String -> Parser ()
printPosition str = do
    p <- Parser.getPosition
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
