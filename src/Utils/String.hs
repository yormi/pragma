module Utils.String
    ( isEmpty
    , mergeLines
    , mergeWords
    , padLeft
    , splitLines
    )
    where

import qualified Data.List as List


isEmpty :: String -> Bool
isEmpty =
    List.null


padLeft :: Int -> String -> String
padLeft targetLength str =
    List.replicate (targetLength - List.length str) ' '
        ++ str


splitLines :: String -> [String]
splitLines =
    List.lines


mergeLines :: [String] -> String
mergeLines =
    List.intercalate "\n"


mergeWords :: [String] -> String
mergeWords =
    List.unwords
