module Type.ValidateAnnotation
    ( Error(..)
    , validate
    ) where

import qualified AST.Module as M


data Error
    = TypeNotInScope
    | TypeArgumentMissing
    | AlreadyDefined
    deriving (Eq, Show)


validate :: M.Module -> [Either Error ()]
validate (M.Module topLevels) =
    []
