module Type.Inference (GatherConstraints, infer) where

--import qualified Data.List as List
import qualified Data.String as String

import AST.Module (Module(..), TopLevel)
import qualified AST.Expression as E
import qualified AST.Module as M
import qualified Type as T
import Type.ConstraintSolver (SolvingError)
import Type.Constraint.Gatherer (Constraint, ConstraintError)
import qualified Type.Constraint.Gatherer as Gatherer
import qualified Type.ConstraintSolver as ConstraintSolver
import qualified Type.Constraint.Module as Module
import qualified Utils.Either as Either


type TypeCheck =
    Module -> [Either TypeError T.Type]


type GatherConstraints =
    TopLevel -> Either ConstraintError [Constraint]


type SolveType =
    [Constraint] -> T.Type -> Either SolvingError ()


data TypeError
    = ConstraintError ConstraintError
    | NoMainFunction
    | SolvingError SolvingError
    deriving (Eq, Show)


-- TODO - REFACTOR !
infer :: TypeCheck
infer (Module topLevels)=
    let
        env =
            map
                (\topLevel ->
                    case topLevel of
                        M.Function { M.functionName, M.type_ } ->
                            (functionName, type_)
                )
                topLevels
    in do
    map
        (gatherConstraints env
            >> Either.mapLeft ConstraintError
            >> printConstraints
            >> bind (ConstraintSolver.solve >> Either.mapLeft SolvingError)
        )
        topLevels


printConstraints :: Either a [Constraint] -> Either a [Constraint]
printConstraints result =
    case result of
        Right cs ->
            (cs
                |> map (show :: Constraint -> String)
                |> String.unlines
                |> trace
            )
            result

        Left _ ->
            result



gatherConstraints :: [(E.Identifier, T.Type)] -> GatherConstraints
gatherConstraints env topLevel =
    Module.gather topLevel
        |> Gatherer.withEnv env
        |> Gatherer.gatherConstraints
