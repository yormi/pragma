module Check.Type.Check.Model.Solution (Solution, SolutionType(..)) where

import qualified Check.Type.Arrange as A
import Check.Type.Check.Model.GenericType (GenericType)
import Check.Type.Check.Model.InstancedType (InstancedType(..))


type Solution
    = Map A.Link SolutionType


data SolutionType
    = Instanced InstancedType
    | Generic GenericType
    deriving (Eq, Show)
