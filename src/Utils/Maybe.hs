module Utils.Maybe
    ( keepValues
    , withDefault
    ) where

import qualified Data.Maybe as Maybe


withDefault :: a -> Maybe a -> a
withDefault =
    Maybe.fromMaybe


keepValues :: [Maybe a] -> [a]
keepValues =
    Maybe.catMaybes
