module Utils.Maybe (withDefault) where

import qualified Data.Maybe as Maybe


withDefault :: a -> Maybe a -> a
withDefault =
    Maybe.fromMaybe
