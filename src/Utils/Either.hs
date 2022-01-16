module Utils.Either
    ( module X
    , fold
    , fromMaybe
    , fromMaybeError
    , map2
    , mapLeft
    , mapRight
    , toMaybe
    , toMaybeError
    , withDefault
    ) where

import qualified Control.Monad as Monad

import qualified Data.Bifunctor as Bifunctor
import qualified Data.Either as Either
import Data.Either as X
    ( isLeft
    , isRight
    , lefts
    , rights
    )


fold :: (a -> c) -> (b -> c) -> Either a b -> c
fold =
    Either.either


map2 :: (a -> b -> c) -> Either e a -> Either e b -> Either e c
map2 =
    Monad.liftM2


mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft =
    Bifunctor.first


mapRight :: (b -> c) -> Either a b -> Either a c
mapRight  =
    Bifunctor.second


fromMaybe :: a -> Maybe b -> Either a b
fromMaybe defaultLeft maybe =
    case maybe of
        Just x ->
            Right x

        Nothing ->
            Left defaultLeft


fromMaybeError :: Maybe error -> Either error ()
fromMaybeError maybe =
    case maybe of
        Just error ->
            Left error

        Nothing ->
            Right ()


toMaybe :: Either a b -> Maybe b
toMaybe e =
    case e of
         Right x ->
             Just x

         Left _ ->
             Nothing


toMaybeError :: Either error a -> Maybe error
toMaybeError either =
    case either of
        Right _ ->
            Nothing

        Left error ->
            Just error


withDefault :: (a -> b) -> Either a b -> b
withDefault f =
    fold f identity
