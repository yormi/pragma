module Type.TypeSolver (solveType) where

import qualified Data.Map as Map

import qualified Type as T
import Type.ConstraintSolver (SolvedType(..), TypeSolution)
import qualified Utils.Either as Either


data TypeSolvingError
    = NoSolutionForTypeVariable T.TypeVariable
    deriving (Eq, Show)


solveType :: TypeSolution -> T.Type -> Either TypeSolvingError SolvedType
solveType solution type_ =
    case type_ of
        T.Bool ->
            return Bool

        T.Int ->
            return Int

        T.Float ->
            return Float

        T.Char ->
            return Char

        T.String ->
            return String

        T.Function (T.FunctionType a b) ->
            Either.map2
                Function
                (solveType solution a)
                (solveType solution b)

        T.Variable v ->
            case Map.lookup v solution of
                Just t ->
                    return t

                Nothing ->
                    Left <| NoSolutionForTypeVariable v


