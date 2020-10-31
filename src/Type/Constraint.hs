module Type.Constraint (gatherConstraints) where

import AST.Module (Module(..))
import qualified Type.Constraint.Gatherer as Gatherer
import Type.Inference (GatherConstraints)
import qualified Type.Constraint.Module as Module


gatherConstraints :: GatherConstraints
gatherConstraints (Module topLevels) =
    traverse Module.gather topLevels
        |> Gatherer.gatherConstraints
