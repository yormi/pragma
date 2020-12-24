module Utils.Maybe
    ( values
    , withDefault
    ) where

import qualified Data.Maybe as Maybe


values :: [Maybe a] -> [a]
values =
    Maybe.catMaybes


withDefault :: a -> Maybe a -> a
withDefault =
    Maybe.fromMaybe
