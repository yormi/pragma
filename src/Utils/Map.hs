module Utils.Map
    ( module X
    , isEmpty
    ) where

import qualified Data.Map as Map

import Data.Map as X
    ( filter
    , fromList
    , keys
    , toList
    )


isEmpty :: Map a b -> Bool
isEmpty =
    Map.null
