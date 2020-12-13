module AST.Module
    ( Module(..)
    , TopLevel(..)
    ) where



import AST.Expression
import AST.CodeQuote (CodeQuote)
import qualified Type as T


data Module =
    Module [ TopLevel ]
    deriving (Eq, Show)


data TopLevel
    = Function
        { codeQuote :: CodeQuote
        , type_ :: T.Type
        , functionName :: Identifier
        , params :: [Identifier]
        , body :: Expr
        }
        deriving (Eq, Show)
