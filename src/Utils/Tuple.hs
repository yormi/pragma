module Utils.Tuple
    ( mapFirst
    , mapSecond
    ) where

mapFirst :: (a -> c) -> (a, b) -> (c, b)
mapFirst f (x, y) =
    (f x, y)


mapSecond :: (b -> c) -> (a, b) -> (a, c)
mapSecond f (x, y) =
    (x, f y)
