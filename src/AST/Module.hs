module AST.Module
    ( DataChoice(..)
    , Module(..)
    , TopLevel(..)
    ) where

import Data.List.NonEmpty (NonEmpty)

import AST.CodeQuote (CodeQuote)
import AST.Expression (QuotedExpression)
import AST.Identifier (ConstructorId, DataId, TypeId)
import AST.TypeAnnotation (TypeAnnotation)


data Module =
    Module [ TopLevel ]
    deriving (Eq, Show)


data TopLevel
    = Function
        { codeQuote :: CodeQuote
        , typeAnnotation :: TypeAnnotation
        , functionName :: DataId
        , params :: [DataId]
        , body :: QuotedExpression
        }
    | SumType
        { codeQuote :: CodeQuote
        , typeName :: TypeId
        , dataChoices :: NonEmpty DataChoice
        }
        deriving (Eq, Show)


data DataChoice =
    DataChoice
        { codeQuote :: CodeQuote
        , tag :: ConstructorId
        , args :: [TypeAnnotation]
        }
        deriving (Eq, Show)
