module Type.Inference (GatherConstraints) where

import AST.Module (Module)
import Type.Constraint.Gatherer (Constraint, ConstraintError)


--type TypeCheck = Program -> Either TypeError SolvedType


type GatherConstraints = Module -> Either ConstraintError [Constraint]


--type SolveType = [Constraint] -> Either SolvingError SolvedType


data SolvedType
    = Bool
    | Char
    | Int
    | Float
    | String
    | Function
        { input :: SolvedType
        , output :: SolvedType
        }
    deriving (Eq, Show)


-- type TypeSolution = Map TypeVariable SolvedType


--infer :: TypeCheck
--infer =
--    ()
