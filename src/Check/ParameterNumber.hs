module Check.TypeArgument
    ( Error(..)
    , validate
    ) where

import qualified AST.Module as M


data Error
    = TooManyParameters
    deriving (Eq, Show)


validate :: M.Module -> [Either Error ()]
validate (M.Module _) =
    []
