module Check.NameClash.Type
    ( Error(..)
    , validate
    ) where

import qualified AST.Module as M


data Error
    = AlreadyDefined
    deriving (Eq, Show)


validate :: M.Module -> [Either Error ()]
validate (M.Module _) =
    []
