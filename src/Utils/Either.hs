module Utils.Either
    ( module X
    , fold
    , map2
    , mapLeft
    , mapRight
    , toMaybe
    , withDefault
    ) where

import qualified Control.Monad as Monad

import qualified Data.Bifunctor as Bifunctor
import Data.Either (either)
import Data.Either as X (lefts, isLeft, rights)


fold :: (a -> c) -> (b -> c) -> Either a b -> c
fold =
    either


map2 :: (a -> b -> c) -> Either e a -> Either e b -> Either e c
map2 =
    Monad.liftM2


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
