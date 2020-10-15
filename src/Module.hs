module Module
    ( Module(..)
    , TopLevel(..)
    ) where



import Expression
import qualified Type as T


data Module =
    Module [ TopLevel ]
    deriving (Eq, Show)


data TopLevel
    = Function
        { type_ :: Maybe T.Type
        , functionName :: Identifier
        , params :: [Identifier]
        , body :: Expr
        }
        deriving (Eq, Show)
