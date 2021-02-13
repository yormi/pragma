module TestUtils
    ( assumeJust
    ) where


import Test.Hspec

import GHC.Err (error)


assumeJust :: HasCallStack => Maybe a -> IO a
assumeJust maybe =
    case maybe of
        Just x ->
            return x

        Nothing -> do
            error "Expected Just !"
