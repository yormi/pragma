module Utils.Tuple (mapFirst) where

mapFirst :: (a -> c) -> (a, b) -> (c, b)
mapFirst f (x, y) =
    (f x, y)
