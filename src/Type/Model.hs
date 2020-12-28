module Type.Model
    ( Kind(..)
    , Type(..)
    , FunctionType(..)
    , TypePlaceholder(..)
    ) where

import AST.Identifier (TypeId, TypeVariableId)


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
    | Variable TypeVariableId
    | Placeholder TypePlaceholder
    | Custom [Type] TypeId
    deriving (Eq, Show)


data FunctionType = FunctionType Type Type
    deriving (Eq, Show)


newtype TypePlaceholder
    = TypePlaceholder Int
    deriving (Eq, Ord, Show)
