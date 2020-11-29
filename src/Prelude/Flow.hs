{-# LANGUAGE NoImplicitPrelude #-}

module Prelude.Flow
    ( module X
    , (|>)
    , (<|)
    , (>>)
    , (<<)
    , apply
    , bind
    , map
    , map2
    ) where

import Control.Monad as X (join)
import Data.Traversable as X
    ( Traversable
    , traverse
    , sequence
    )


import Control.Applicative (Applicative, (<**>))
import qualified Control.Applicative as Applicative
import Control.Monad (Monad, (=<<))
import Data.Function (($), (&), (.), flip)
import Data.Functor (Functor, fmap)


infixl 0 |>
(|>) :: a -> (a -> b) -> b
(|>) = (&)


infixr 0 <|
(<|) :: (a -> c) -> a -> c
(<|) = ($)


infixl 0 >>
(>>) :: (a -> b) -> (b -> c) -> a -> c
(>>) = flip (.)


infixr 0 <<
(<<) :: (b -> c) -> (a -> b) -> a -> c
(<<) = (.)


apply :: Applicative f => f a -> f (a -> b) -> f b
apply =
    (<**>)


bind :: Monad m => (a -> m b) -> m a -> m b
bind =
    (=<<)


map :: Functor f => (a -> b) -> f a -> f b
map =
    fmap


map2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
map2 =
    Applicative.liftA2
