module Check.Type.Check.Model.GenericType
    ( GenericType(..)
    , VariableId
    ) where


import AST.Identifier (TypeId, TypeVariableId)
import Check.Type.Check.Model.InstancedType (InstanceId)


data GenericType
    = Bool
    | Int
    | Float
    | Char
    | String
    | Function
        { arg :: GenericType
        , returnType :: GenericType
        }
    | Custom
        { typeName :: TypeId
        , args :: [GenericType]
        }
    | ParentVariable TypeVariableId InstanceId
    | Variable VariableId
    deriving (Eq, Show)


type VariableId =
    Int
