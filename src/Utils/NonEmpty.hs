module Utils.NonEmpty (build) where

import Data.List.NonEmpty (NonEmpty(..))

build :: a -> [a] -> NonEmpty a
build x xs =
    x :| xs
