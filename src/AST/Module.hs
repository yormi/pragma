module AST.Module
    ( DataChoice(..)
    , Field(..)
    , Module(..)
    , TopLevel(..)
    ) where

import Data.List.NonEmpty (NonEmpty)

import Parser.Model.Position (Position)
import Parser.Model.Quote (Quote)
import AST.Expression (Expression)
import AST.Identifier (ConstructorId, DataId, TypeId, TypeVariableId)
import AST.TypeAnnotation (TypeAnnotation)
import Utils.OrderedSet (OrderedSet)


newtype Module =
    Module [ TopLevel ]
    deriving (Eq, Show)


data TopLevel
    = Function
        { quote :: Quote
        , typeAnnotation :: TypeAnnotation
        , functionName :: DataId
        , params :: [DataId]
        , body :: Expression
        }
    | SumType
        { fromPosition :: Position
        , typeName :: TypeId
        , typeVariables :: OrderedSet TypeVariableId
        , dataChoices :: NonEmpty DataChoice
        }
    | Record
        { quote :: Quote
        , typeName :: TypeId
        , typeVariables :: OrderedSet TypeVariableId -- TODO - Make sure to prevent if a type variable appear more than once `SumType a a`
        , fields :: NonEmpty Field
        }
        deriving (Eq, Show)


data Field
    = Field
        { quote :: Quote
        , name :: DataId
        , typeAnnotation :: TypeAnnotation
        }
        deriving (Eq, Show)


data DataChoice
    = DataChoice
        { tag :: ConstructorId
        , args :: [TypeAnnotation]
        }
        deriving (Eq, Show)
