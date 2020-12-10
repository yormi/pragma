module Utils.List
    ( module X
    , head
    , isEmpty
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
    )


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


slice :: Int -> Int -> [a] -> [a]
slice from to =
    List.drop from
        >> List.take (to - from + 1)
