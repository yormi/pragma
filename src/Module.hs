module Module
    ( Module(..)
    , TopLevel(..)
    ) where


import Data.List.NonEmpty (NonEmpty)

import Expression
import qualified Type as T


data Module =
    Module [ TopLevel ]
    deriving (Eq, Show)


data TopLevel
    = Function
        { type_ :: Maybe T.Type
        , functionName :: Identifier
        , params :: NonEmpty Identifier
        , body :: Expr
        }
        deriving (Eq, Show)
