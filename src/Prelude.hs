module Prelude
    ( module X
    , identity
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
    , maybe
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

import Control.Applicative as X
    ( Applicative
    , pure
    )


import Control.Monad as X
    ( Monad
    , return
    , fail
    )

import Data.Either as X
    ( Either(..)
    )

import Data.Ord as X (Ord(..))

import Data.Semigroup as X ((<>))

--import Data.Traversable as X
--    ( traverse
--    )


import System.IO as X
    ( print
    )


import Prelude.Flow as X
import Prelude.String as X


identity :: a -> a
identity x =
    x
