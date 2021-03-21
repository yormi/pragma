module Utils.Map
    ( module X
    , isEmpty
    ) where

import qualified Data.Map as Map

import Data.Map as X
    ( empty
    , filter
    , fromList
    , insert
    , keys
    , lookup
    , toList
    , unions
    )


isEmpty :: Map a b -> Bool
isEmpty =
    Map.null
