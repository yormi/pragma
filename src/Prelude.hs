module Prelude
    ( module X
    , identity
    , toFloat
    , toInt
    ) where


import Relude as X
    ( Bool(..)
    , Eq(..)
    , Float
    , Double
    , Generic
    , Int
    , Integer
    , IO
    , Maybe(..)
    , Show
    , String
    , FilePath
    , (+)
    , (-)
    , (++)
    , (*)
    , (/)
    , (&&)
    , (||)
    , div
    , fst
    , round
    , snd
    --, mempty
    , not
    )

import Relude.Function as X
    ( const
    , flip
    )

import Relude.Functor as X
    ( Functor(..)
    , void
    )

import Relude.Numeric (fromIntegral)

import Control.Applicative as X
    ( Applicative
    , pure
    )


import Control.Monad as X
    ( Monad
    , return
    )

import Control.Monad.Trans.Class as X
    ( lift
    )

import Data.Either as X
    ( Either(..)
    )

import Data.Ord as X (Ord(..))

import Data.Semigroup as X ((<>))

--import Data.Traversable as X
--    ( traverse
--    )


--import System.IO as X
--    ( print
--    )


import Prelude.Flow as X
import Prelude.String as X


identity :: a -> a
identity x =
    x


toFloat :: Int -> Float
toFloat  =
    fromIntegral


toInt :: Float -> Int
toInt =
    round
