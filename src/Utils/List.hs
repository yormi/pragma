module Utils.List
    ( module X
    , contains
    , head
    , isEmpty
    , unique
    , slice
    ) where

import qualified Data.List as List

import Data.List as X
    ( concat
    , filter
    , foldl
    , length
    , replicate
    , reverse
    , zip
    )


contains :: Eq a => a -> [a] -> Bool
contains =
    List.elem


head :: [a] -> Maybe a
head xs =
    case xs of
         x : _ ->
             Just x

         [] ->
             Nothing


isEmpty :: [a] -> Bool
isEmpty =
    List.null


unique :: Eq a => [a] -> [a]
unique =
    List.nub


slice :: Int -> Int -> [a] -> [a]
slice from to =
    List.drop from
        >> List.take (to - from + 1)
