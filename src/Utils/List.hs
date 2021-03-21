module Utils.List
    ( module X
    , contains
    , head
    , indexedMap
    , isEmpty
    , last
    , range
    , slice
    , singleton
    , unique
    ) where

import qualified Data.List as List

import Data.List as X
    ( any
    , all
    , concat
    , drop
    , dropWhileEnd
    , filter
    , foldl
    , groupBy
    , intercalate
    , intersperse
    , length
    , replicate
    , reverse
    , take
    , takeWhile
    , zip
    , zip3
    )
import Data.Foldable as X
    ( find
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


last :: [a] -> Maybe a
last xs =
    if isEmpty xs then
        Nothing

    else
        Just <| List.last xs


unique :: Eq a => [a] -> [a]
unique =
    List.nub


range :: Int -> Int -> [Int]
range from toExclusively =
    let
        wantedLength =
            toExclusively - from
    in
    if wantedLength > 0 then
        List.replicate wantedLength (1 :: Int)
            |> indexedMap (\index _ -> index + from)

    else
        []


singleton :: a -> [a]
singleton x =
    [x]


slice :: Int -> Int -> [a] -> [a]
slice from to =
    List.drop from
        >> List.take (to - from + 1)
