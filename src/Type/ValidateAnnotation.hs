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
validate (M.Module _) =
    []
    -- Validate that custom type defined the variables used by constructors!
