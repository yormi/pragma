module Type.Constraint.Gatherer.Gather (gather) where

import qualified AST.Module as M
import qualified Type.Constraint.Gatherer.Context as ContextGatherer
import qualified Type.Constraint.Gatherer.Model as Gatherer
import Type.Constraint.Model (Constraint)
import qualified Type.Constraint.Gatherer.Module as Module


gather :: M.Module -> M.TopLevel -> Either Gatherer.ConstraintError [Constraint]
gather module_ topLevel =
    Gatherer.gatherConstraints <| do
        context <- ContextGatherer.gatherer module_
        Gatherer.withContext context <|
            Module.gather topLevel

