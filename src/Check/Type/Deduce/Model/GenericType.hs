module Check.Type.Deduce.Model.GenericType
    ( GenericType(..)
    ) where


import AST.Identifier (TypeId, TypeVariableId)


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
        { name :: TypeId
        , args :: [GenericType]
        }
    | Variable TypeVariableId
    deriving (Eq, Show)
    --  | ParentVariable TypeVariableId T.TypePlaceholder
    --  | Placeholder T.TypePlaceholder
