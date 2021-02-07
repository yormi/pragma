module AST3.Module
    ( DataChoice(..)
    , Field(..)
    , Module(..)
    , TopLevel(..)
    ) where

import Data.List.NonEmpty (NonEmpty)

import AST.CodeQuote (CodeQuote)
import AST.Expression (QuotedExpression)
import AST.Identifier (ConstructorId, DataId, TypeId, TypeVariableId)
import AST.TypeAnnotation (TypeAnnotation)
import Utils.OrderedSet (OrderedSet)


newtype Module =
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
        , typeVariables :: OrderedSet TypeVariableId -- TODO - Make sure to prevent if a type variable appear more than once `SumType a a`
        , dataChoices :: NonEmpty DataChoice
        }
    | Record
        { codeQuote :: CodeQuote
        , typeName :: TypeId
        , typeVariables :: OrderedSet TypeVariableId -- TODO - Make sure to prevent if a type variable appear more than once `SumType a a`
        , fields :: NonEmpty Field
        }
        deriving (Eq, Show)


data Field
    = Field
        { codeQuote :: CodeQuote
        , name :: DataId
        , typeAnnotation :: TypeAnnotation
        }
        deriving (Eq, Show)


data DataChoice
    = DataChoice
        { codeQuote :: CodeQuote
        , tag :: ConstructorId
        , args :: [TypeAnnotation]
        }
        deriving (Eq, Show)
