module Check.Type.Check.Model.InstancedType
    ( InstanceId
    , InstancedType(..)
    , UnboundId
    ) where

import AST.Identifier (TypeId)


data InstancedType
    = Bool
    | Int
    | Float
    | Char
    | String
    | Function
        { arg :: InstancedType
        , returnType :: InstancedType
        }
    | Custom
        { typeId :: TypeId
        , args :: [InstancedType]
        }
    | Unbound UnboundId
    | Instance InstanceId
    deriving (Eq, Show)


type UnboundId =
    Int


type InstanceId =
    Int
