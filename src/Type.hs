module Type
    ( Type(..)
    , FunctionType(..)
    , VariableNumber
    ) where


data Type
    = Bool
    | Int
    | Float
    | Char
    | String
    | Function FunctionType
    | Variable VariableNumber
    deriving (Eq, Show)


data FunctionType = FunctionType Type Type
    deriving (Eq, Show)


type VariableNumber = Int
