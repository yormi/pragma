module Utils.List
    ( module X
    , head
    , isEmpty
    ) where

import qualified Data.List as List

import Data.List as X
    (filter
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
