module Type
    ( Type(..)
    , FunctionType(..)
    , TypeVariable
    ) where


data Type
    = Bool
    | Int
    | Float
    | Char
    | String
    | Function FunctionType
    | Variable TypeVariable
    deriving (Eq, Show)


data FunctionType = FunctionType Type Type
    deriving (Eq, Show)


type TypeVariable = Int
