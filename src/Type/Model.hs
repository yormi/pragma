module Type.Model
    ( Kind(..)
    , Type(..)
    , FunctionType(..)
    , TypePlaceholder(..)
    ) where

import AST.Identifier (TypeId, TypeVariableId)
import Utils.OrderedSet (OrderedSet)


data Kind
    = Kind
        { typeVariables :: [TypeVariableId]
        , typeId :: TypeId
        }
    deriving (Eq, Show)


data Type
    = Bool
    | Int
    | Float
    | Char
    | String
    | Function FunctionType
    | Placeholder TypePlaceholder
    | Custom TypeId (OrderedSet Type)
    deriving (Eq, Ord, Show)


data FunctionType = FunctionType Type Type
    deriving (Eq, Ord, Show)


newtype TypePlaceholder
    = TypePlaceholder Int
    deriving (Eq, Ord, Show)
