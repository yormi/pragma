module Utils.OrderedSet
    ( OrderedSet
    , empty
    , fromList
    , toList
    ) where

import qualified Utils.List as List

newtype OrderedSet a =
    OrderedSet [a]
        deriving (Eq, Ord, Show)


empty :: OrderedSet a
empty =
    OrderedSet []


fromList :: Eq a => [a] -> OrderedSet a
fromList =
    List.unique
        >> OrderedSet


toList :: OrderedSet a -> [a]
toList (OrderedSet xs) =
    xs
