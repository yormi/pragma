module Type.Inference (TypeError(..), gatherConstraints, infer) where

import qualified Data.Bifunctor as Bifunctor
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.String as String

import AST.Module (Module(..), TopLevel)
-- import qualified AST.Expression as E
import qualified AST.Module as M
import qualified Printer as TypePrinter
-- import qualified Type as T
import Type.ConstraintSolver (SolvingError)
import Type.Constraint.Gatherer (Constraint, ConstraintError)
import qualified Type.Constraint.Gatherer as Gatherer
import qualified Type.Constraint.Printer as Printer
import Type.ConstraintSolver (TypeSolution)
import qualified Type.ConstraintSolver as ConstraintSolver
import qualified Type.Constraint.Module as Module
import qualified Utils.Either as Either


type TypeCheck =
    Module -> [Either TypeError ConstraintSolver.TypeSolution]


data TypeError
    = ConstraintError ConstraintError
    | NoMainFunction
    | SolvingError SolvingError
    deriving (Eq, Show)


-- TODO - REFACTOR !
infer :: TypeCheck
infer module_@(Module topLevels)=
    map
        (gatherConstraints module_
            >> Either.mapLeft ConstraintError
            >> traceConstraints
            >> trace "---"
            >> bind (ConstraintSolver.solve >> Either.mapLeft SolvingError)
            >> traceResult
        )
        topLevels


traceResult :: Either TypeError TypeSolution -> Either TypeError TypeSolution
traceResult =
    map
        (\solution ->
            let
                solutions =
                    solution
                        |> Map.toList
                        |> map (Bifunctor.first (\n -> "a" ++ show n))
                        |> map
                            (\(variable, type_) ->
                                variable
                                    ++ " :: "
                                    ++ TypePrinter.printType type_
                            )
                        |> List.intercalate "\n\t"
            in
            trace ("Result:\n\t" ++ solutions) solution
        )
        >> Either.mapLeft (\e -> trace ("Result:\n\t" ++ show e) e)


traceConstraints :: Either a [Constraint] -> Either a [Constraint]
traceConstraints =
    map
        (\constraints ->
            constraints
                |> map (Printer.printConstraint)
                |> String.unlines
                |> flip trace constraints
        )



gatherConstraints :: Module -> TopLevel -> Either ConstraintError [Constraint]
gatherConstraints (Module topLevels) topLevel =
    let
        env =
            map
                (\t ->
                    case t of
                        M.Function { M.functionName, M.type_ } ->
                            (functionName, type_)
                )
                topLevels
    in do
    Module.gather topLevel
        |> Gatherer.withEnv env
        |> Gatherer.gatherConstraints
