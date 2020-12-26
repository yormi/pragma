module Utils.Maybe
    ( toList
    , values
    , withDefault
    ) where

import qualified Data.Maybe as Maybe


toList :: Maybe a -> [a]
toList maybe =
    case maybe of
        Just x ->
            [x]

        Nothing ->
            []


values :: [Maybe a] -> [a]
values =
    Maybe.catMaybes


withDefault :: a -> Maybe a -> a
withDefault =
    Maybe.fromMaybe
