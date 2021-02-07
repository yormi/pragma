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
    , splitOn
    , toInt
    )
    where

import Data.List as X (length)
import qualified Data.List as List
import qualified Text.Read as Read

import Prelude hiding (toInt)


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


splitOn :: Char -> String -> [String]
splitOn c str =
    let
        predicate =
            (==) c

        afterChar =
            List.dropWhile predicate str
    in
    if isEmpty afterChar then
        []

    else
        let
            (before, rest2) =
                List.break predicate afterChar
        in
        before : splitOn c rest2


mergeLines :: [String] -> String
mergeLines =
    List.intercalate "\n"


mergeWords :: [String] -> String
mergeWords =
    List.unwords


toInt :: String -> Maybe Int
toInt =
    Read.readMaybe
