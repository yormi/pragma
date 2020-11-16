module Type.ConstraintSolver
    ( SolvingError
    , TypeSolution
    , solve
    ) where

import Control.Monad.State (StateT)
import qualified Control.Monad.State as State
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map (Map)
import qualified Data.Map as Map

import qualified Type as T
import Type.Constraint.Model (Constraint(..))
import qualified Utils.Maybe as Maybe


type Solver a = StateT TypeSolution (Either SolvingError) a


type TypeSolution = Map T.TypeVariable T.Type


data SolvingError
    = UnsolvableConstraint T.Type T.Type
    | NotAFunction
    deriving (Eq, Show)


fail :: SolvingError -> Solver a
fail e =
    lift <| Left e


mostPrecised :: T.Type -> Solver T.Type
mostPrecised type_ =
    case type_ of
        T.Variable v -> do
            state <- State.get
            closest <-
                Map.lookup v state
                    |> traverse mostPrecised
            closest
                |> Maybe.withDefault type_
                |> return

        _ ->
            return type_


updateTypeSolution :: T.TypeVariable -> T.Type -> Solver ()
updateTypeSolution typeVariable concludedType = do
    state <- State.get

    case Map.lookup typeVariable state of
        Nothing ->
            addToSolution typeVariable concludedType

        Just (T.Variable newTypeVariable) ->
            updateTypeSolution newTypeVariable concludedType

        Just a ->
            if a == concludedType then
                return ()
            else
                -- TODO - Better error
                fail <| UnsolvableConstraint a concludedType


addToSolution :: T.TypeVariable -> T.Type -> Solver ()
addToSolution v t =
    State.modify (Map.insert v t)


------------------


solve :: [Constraint] -> Either SolvingError TypeSolution
solve constraints =
    traverse solveConstraint constraints
        |> processSolution


processSolution :: Solver a -> Either SolvingError TypeSolution
processSolution =
    flip State.execStateT Map.empty


solveConstraint :: Constraint -> Solver ()
solveConstraint constraint =
    case constraint of
        Simple a b ->
            solveSimple a b


        IfThenElse { condition, whenTrue, whenFalse, returnType } -> do
            solveSimple condition T.Bool
            solveSimple whenTrue whenFalse
            solveSimple returnType whenFalse


        Application { functionReference, args, returnType } ->
            buildFunction (NonEmpty.toList args) returnType
                |> solveSimple functionReference


        Function { functionType, params, body } ->
            buildFunction params body
                |> solveSimple functionType


buildFunction :: [T.Type] -> T.Type -> T.Type
buildFunction params finalType =
    List.foldl
        (\builtType p ->
            T.Function <| T.FunctionType p builtType
        )
        finalType
        (List.reverse params)


solveSimple :: T.Type -> T.Type -> Solver ()
solveSimple a b = do
    precisedA <- mostPrecised a
    precisedB <- mostPrecised b

    case (precisedA, precisedB) of
        (T.Function f, T.Function g) ->
            solveFunction f g

        (T.Variable _, T.Variable _) -> do
            fail <| UnsolvableConstraint a b

        (T.Variable variableA, _) -> do
            updateTypeSolution variableA b

        (_, T.Variable variableB) -> do
            updateTypeSolution variableB a

        _ ->
            if precisedA == precisedB then
                return ()
            else
                fail <| UnsolvableConstraint a b


solveFunction :: T.FunctionType -> T.FunctionType -> Solver ()
solveFunction (T.FunctionType argA returnA) (T.FunctionType argB returnB) = do
    a <- mostPrecised argA
    b <- mostPrecised argB

    solveSimple a b
    solveSimple returnA returnB
