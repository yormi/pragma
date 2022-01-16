module Check.Type.Model.Type
    ( InstancedType(..)
    , Type(..)
    ) where


data Type
    = Bool
    | Int
    | Float
    | Char
    | String
    | Function
        { arg :: Type
        , returning :: Type
        }
    | Unbound
        { name :: String
        , instanceType :: InstancedType
        }
    | Custom
        { name :: String
        , args :: [Type]
        }
    deriving (Eq, Show)



newtype UnboundType =
    UnboundType Int
        deriving (Eq, Show)


newtype InstancedType =
    InstancedType Int
        deriving (Eq, Show)
