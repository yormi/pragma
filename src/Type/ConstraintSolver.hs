module Type.ConstraintSolver
    ( SolvingError
    , TypeSolution
    , solve
    ) where

import Control.Monad.State (StateT)
import qualified Control.Monad.State as State
import Data.Map (Map)
import qualified Data.Map as Map

import qualified Type as T
import Type.Constraint.Model (Constraint(..))


type Solver a = StateT TypeSolution (Either SolvingError) a


type TypeSolution = Map T.TypeVariable T.Type


data SolvingError
    = UnsolvableConstraint T.Type T.Type
    deriving (Eq, Show)


fail :: SolvingError -> Solver a
fail e =
    lift <| Left e


addToSolution :: T.TypeVariable -> T.Type -> Solver ()
addToSolution v t =
    State.modify (Map.insert v t)


inferedType :: T.TypeVariable -> Solver (Maybe T.Type)
inferedType v =
    State.get
        |> map (Map.lookup v)


updateTypeSolution :: T.TypeVariable -> T.Type -> Solver ()
updateTypeSolution typeVariable concludedType = do
    infered <- inferedType typeVariable
    case infered of
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


------------------


solve :: [Constraint] -> Either SolvingError T.Type
solve constraints =
    solveConstraints constraints
        |> processSolution
        |> const (return T.Bool)


processSolution :: Solver a -> Either SolvingError TypeSolution
processSolution =
    flip State.execStateT Map.empty


solveConstraints :: [Constraint] -> Solver ()
solveConstraints constraints =
    case constraints of
        [] ->
            return ()

        c : cs -> do
           solveConstraint c
           solveConstraints cs


solveConstraint :: Constraint -> Solver ()
solveConstraint constraint =
    case constraint of
        Simple (T.Variable a) (T.Variable b) -> do
            inferedA <- inferedType a
            inferedB <- inferedType b

            case (inferedA, inferedB) of
                (Just newA, Just newB) ->
                    solveConstraint <| Simple newA newB

                (Just newA, Nothing) ->
                    solveConstraint <| Simple newA (T.Variable b)

                (Nothing, Just newB) ->
                    solveConstraint <| Simple (T.Variable a) newB

                (Nothing, Nothing) -> do
                    -- TODO - Better error
                    fail <| UnsolvableConstraint (T.Variable a) (T.Variable b)


        Simple (T.Variable a) b -> do
            inferedA <- inferedType a
            case inferedA of
                Just newA ->
                    solveConstraint <| Simple newA b

                Nothing ->
                    updateTypeSolution a b


        -- TODO - Handle Function in here then switch this to a module called
        -- by the ConstraintGatherer (that we should rename Infer i guess)


        Simple a b ->
            if a == b then
                return ()
            else
                fail <| UnsolvableConstraint a b

-- \ a b ->
--      if True then
--          a
--      else
--          b
