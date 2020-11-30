module Utils.String
    ( mergeLines
    , padLeft
    , splitLines
    )
    where

import qualified Data.List as List


padLeft :: Int -> String -> String
padLeft targetLength str =
    List.replicate (targetLength - List.length str) ' '
        ++ str


splitLines :: String -> [String]
splitLines =
    List.lines


mergeLines :: [String] -> String
mergeLines =
    List.unlines
