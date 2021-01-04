module Type.Constraint.Solver.Model.Solution
    ( InstancedType(..)
    , Solution
    , SolutionType(..)
    ) where

import Type.Constraint.Reference (Reference)
import Type.Constraint.Solver.Model.Instanced (InstancedType(..))
import Type.Constraint.Solver.Model.Generic (GenericType)
import qualified Type.Model as T


type Solution
    = Map T.TypePlaceholder SolutionType


data SolutionType
    = Instanced InstancedType
    | Generic
        { reference :: Reference
        , type_ :: GenericType
        }
    deriving (Eq, Show)
