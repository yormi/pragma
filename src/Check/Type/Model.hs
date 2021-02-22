module Check.Type.Model
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
    -- | Placeholder TypePlaceholder
    | Custom
        { name :: String
        , args :: [Type]
        }
    deriving (Eq, Show)


newtype InstancedType =
    InstancedType Int
        deriving (Eq, Show)
