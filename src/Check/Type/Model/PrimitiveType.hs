module Check.Type.Model.PrimitiveType (Type(..)) where


data Type
    = Bool
    | Int
    | Float
    | Char
    | String
    deriving (Eq, Show)
