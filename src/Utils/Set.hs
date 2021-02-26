module Utils.Set
    ( module X
    , contains
    , isEmpty
    , singleton
    ) where

import qualified Data.Set as Set

import Data.Set as X
    ( empty
    , filter
    , fromList
    , toList
    )


contains :: Ord a => a -> Set a -> Bool
contains =
    Set.member


isEmpty :: Set a -> Bool
isEmpty =
    Set.null


singleton :: Ord a => a -> Set a
singleton x =
    Set.fromList [ x ]
