module Utils.List
    ( module X
    , contains
    , head
    , indexedMap
    , isEmpty
    , unique
    , slice
    , singleton
    ) where

import qualified Data.List as List

import Data.List as X
    ( any
    , concat
    , filter
    , foldl
    , intercalate
    , intersperse
    , length
    , replicate
    , reverse
    , zip
    , zip3
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


indexedMap :: (Int -> a -> b) -> [a] -> [b]
indexedMap f xs =
    xs
        |> List.foldl
            (\(index, result) x -> (index + 1, f index x : result))
            (0, [])
        |> snd
        |> List.reverse


isEmpty :: [a] -> Bool
isEmpty =
    List.null


unique :: Eq a => [a] -> [a]
unique =
    List.nub


singleton :: a -> [a]
singleton x =
    [x]


slice :: Int -> Int -> [a] -> [a]
slice from to =
    List.drop from
        >> List.take (to - from + 1)
