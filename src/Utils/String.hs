module Utils.String
    ( containsChar
    , containsString
    , isEmpty
    , length
    , mergeLines
    , mergeWords
    , padLeft
    , padRight
    , splitLines
    )
    where

import Data.List as X (length)
import qualified Data.List as List


containsChar :: Char -> String -> Bool
containsChar =
    List.elem


containsString :: String -> String -> Bool
containsString =
    List.isInfixOf


isEmpty :: String -> Bool
isEmpty =
    List.null


padLeft :: Int -> String -> String
padLeft targetLength str =
    List.replicate (targetLength - List.length str) ' '
        ++ str


padRight :: Int -> String -> String
padRight targetLength str =
    str
        ++ List.replicate (targetLength - List.length str) ' '


splitLines :: String -> [String]
splitLines =
    List.lines


mergeLines :: [String] -> String
mergeLines =
    List.intercalate "\n"


mergeWords :: [String] -> String
mergeWords =
    List.unwords
