module Utils.Either
    ( module X
    , fold
    , mapLeft
    , mapRight
    , toMaybe
    , withDefault
    ) where

import qualified Data.Bifunctor as Bifunctor
import Data.Either (either)
import Data.Either as X (lefts, isLeft, rights)


fold :: (a -> c) -> (b -> c) -> Either a b -> c
fold =
    either


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
