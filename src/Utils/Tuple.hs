module Utils.Tuple
    ( first
    , mapFirst
    , mapSecond
    , second
    ) where


first :: (a, b) -> a
first =
    fst


second :: (a, b) -> b
second =
    snd


mapFirst :: (a -> c) -> (a, b) -> (c, b)
mapFirst f (x, y) =
    (f x, y)


mapSecond :: (b -> c) -> (a, b) -> (a, c)
mapSecond f (x, y) =
    (x, f y)
