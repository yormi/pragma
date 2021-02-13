module AST3.Module
    ( DataChoice(..)
    , Field(..)
    , Module(..)
    , TopLevel(..)
    ) where

import Data.List.NonEmpty (NonEmpty)

import Parser3.Model.Quote (Quote)
import AST3.Expression (Expression)
import AST3.Identifier (ConstructorId, DataId, TypeId, TypeVariableId)
import AST3.TypeAnnotation (TypeAnnotation)
import Utils.OrderedSet (OrderedSet)


newtype Module =
    Module [ TopLevel ]
    deriving (Eq, Show)


data TopLevel
    = Function
        { codeQuote :: Quote
        , typeAnnotation :: TypeAnnotation
        , functionName :: DataId
        , params :: [DataId]
        , body :: Expression
        }
    | SumType
        { codeQuote :: Quote
        , typeName :: TypeId
        , typeVariables :: OrderedSet TypeVariableId -- TODO - Make sure to prevent if a type variable appear more than once `SumType a a`
        , dataChoices :: NonEmpty DataChoice
        }
    | Record
        { codeQuote :: Quote
        , typeName :: TypeId
        , typeVariables :: OrderedSet TypeVariableId -- TODO - Make sure to prevent if a type variable appear more than once `SumType a a`
        , fields :: NonEmpty Field
        }
        deriving (Eq, Show)


data Field
    = Field
        { codeQuote :: Quote
        , name :: DataId
        , typeAnnotation :: TypeAnnotation
        }
        deriving (Eq, Show)


data DataChoice
    = DataChoice
        { codeQuote :: Quote
        , tag :: ConstructorId
        , args :: [TypeAnnotation]
        }
        deriving (Eq, Show)
