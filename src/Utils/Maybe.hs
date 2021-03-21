module Utils.Maybe
    ( module X
    , map2
    , toList
    , values
    , withDefault
    ) where

import qualified Control.Monad as Monad
import qualified Data.Maybe as Maybe
import Data.Maybe as X
    ( isJust
    )


map2 :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
map2 =
    Monad.liftM2


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
