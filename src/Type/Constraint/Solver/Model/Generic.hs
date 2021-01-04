module Type.Constraint.Solver.Model.Generic
    ( GenericType(..)
    ) where


import AST.Identifier (TypeId, TypeVariableId)
import qualified Type.Model as T


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
    | ParentVariable TypeVariableId T.TypePlaceholder
    | Variable TypeVariableId
    | Placeholder T.TypePlaceholder
    deriving (Eq, Show)
