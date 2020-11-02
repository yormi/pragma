module Type.Inference (GatherConstraints, infer) where

import qualified Data.List as List
import qualified Data.String as String

import AST.Module (Module(..), TopLevel)
import qualified AST.Expression as E
import qualified AST.Module as M
import qualified Type as T
import Type.Constraint.Gatherer (Constraint, ConstraintError)
import qualified Type.Constraint.Gatherer as Gatherer
import qualified Type.Constraint.Module as Module
import qualified Utils.Either as Either


type TypeCheck =
    Module -> [Either TypeError SolvedType]


type GatherConstraints =
    TopLevel -> Either ConstraintError (T.Type, [Constraint])


type SolveType =
    [Constraint] -> T.Type -> Either SolvingError SolvedType


data TypeError
    = ConstraintError ConstraintError
    | NoMainFunction
    | SolvingError SolvingError
    deriving (Eq, Show)


data SolvingError
    = A
    deriving (Eq, Show)


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
            >> bind
                (\(type_, constraints) ->
                    type_
                        |> solveType constraints
                        |> Either.mapLeft SolvingError
                )
        )
        topLevels


printConstraints :: Either a (b, [Constraint]) -> Either a (b, [Constraint])
printConstraints x =
    case x of
        Right (_, cs) ->
            (cs
                |> map (show :: Constraint -> String)
                -- |> List.intersperse "\n"
                |> String.unlines
                |> trace
            )
                x

        Left _ ->
            x



gatherConstraints :: [(E.Identifier, T.Type)] -> GatherConstraints
gatherConstraints env topLevel =
    Module.gather topLevel
        |> Gatherer.withEnv env
        |> Gatherer.run


solveType :: SolveType
solveType _ _ =
    Right Bool
