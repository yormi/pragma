module Utils.Either
    ( fold
    , map2
    , mapLeft
    , mapRight
    , toMaybe
    , withDefault
    ) where

import qualified Control.Applicative as Applicative
import qualified Data.Bifunctor as Bifunctor
import Data.Either (either)


fold :: (a -> c) -> (b -> c) -> Either a b -> c
fold =
    either


map2 :: (a -> b -> c) -> Either err a -> Either err b -> Either err c
map2  =
    Applicative.liftA2


mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft =
    Bifunctor.first


mapRight :: (b -> c) -> Either a b -> Either a c
mapRight  =
    Bifunctor.second


toMaybe :: Either a b -> Maybe b
toMaybe e =
    case e of
         Right x ->
             Just x

         Left _ ->
             Nothing


withDefault :: (a -> b) -> Either a b -> b
withDefault f =
    either f identity
