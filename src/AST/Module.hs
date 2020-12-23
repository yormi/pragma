module AST.Module
    ( DataChoice(..)
    , Module(..)
    , TopLevel(..)
    ) where

import Data.List.NonEmpty (NonEmpty)

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
    | SumType
        { codeQuote :: CodeQuote
        , typeName :: String
        , dataChoices :: NonEmpty DataChoice
        }
        deriving (Eq, Show)


data DataChoice =
    DataChoice
        { codeQuote :: CodeQuote
        , tag :: String
        , args :: [TypeAnnotation]
        }
        deriving (Eq, Show)
