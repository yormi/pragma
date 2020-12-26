module Type.Model
    ( Type(..)
    , FunctionType(..)
    , TypeVariable
    ) where

import AST.Identifier (TypeId)


data Type
    = Bool
    | Int
    | Float
    | Char
    | String
    | Function FunctionType
    | Variable TypeVariable
    | Placeholder TypeVariable
    | Custom TypeId
    deriving (Eq, Show)


data FunctionType = FunctionType Type Type
    deriving (Eq, Show)


type TypeVariable = Int
