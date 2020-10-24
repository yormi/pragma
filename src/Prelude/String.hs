{-# LANGUAGE NoImplicitPrelude #-}

module Prelude.String
    ( module X
    , pipeTrace
    , trace
    )
    where


import Data.Char as X
    (Char
    )

import Data.Text as X
    ( Text
    , null
    , unwords
    )


import System.IO as X
    ( putStrLn
    )


import Relude.String.Conversion as X
    ( LText
    , LByteString
    , ConvertUtf8(..)
    , LazyStrict(..)
    , ToString(..)
    , ToLText(..)
    , ToText(..)
    , fromLazy
    , fromStrict
    , toText
    , show
    )



import Data.Semigroup ((<>))
import qualified Debug.Trace as Debug
import Relude (String)
import qualified Relude

import Prelude.Flow ((>>))


trace :: String -> b -> b
trace =
    toString >> Debug.trace


pipeTrace :: Relude.Show a => String -> a -> a
pipeTrace description x =
    trace (description <> ": " <> show x ) x
{-# WARNING pipeTrace "'pipeTrace remains in code" #-}
