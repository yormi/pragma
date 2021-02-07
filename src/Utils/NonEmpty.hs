module Utils.NonEmpty (module X, build, fromList) where

import Data.List.NonEmpty (NonEmpty(..), nonEmpty)
import Data.List.NonEmpty as X (NonEmpty(..), last, tail, toList)


build :: a -> [a] -> NonEmpty a
build x xs =
    x :| xs


fromList :: [a] -> Maybe (NonEmpty a)
fromList =
    nonEmpty
