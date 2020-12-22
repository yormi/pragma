module AST.Module
    ( Module(..)
    , TopLevel(..)
    ) where


import AST.Expression (Identifier, QuotedExpression)
import AST.CodeQuote (CodeQuote)
import AST.TypeAnnotation (TypeAnnotation)


data Module =
    Module [ TopLevel ]
    deriving (Eq, Show)


data TopLevel
    = Function
        { codeQuote :: CodeQuote
        , typeAnnotation :: TypeAnnotation
        , functionName :: Identifier
        , params :: [Identifier]
        , body :: QuotedExpression
        }
        deriving (Eq, Show)
