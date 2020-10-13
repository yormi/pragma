module Type
    ( Type(..)
    ) where


data Type
    = Bool
    | Int
    | Float
    | Char
    | String
    -- | TypeBuilder1 Type Type
    -- | TypeBuilder2 Type Type Type
    -- | TypeBuilder3 Type Type Type Type
    -- | TypeBuilder4 Type Type Type Type Type
    | Function Type Type
    deriving (Eq, Show)
