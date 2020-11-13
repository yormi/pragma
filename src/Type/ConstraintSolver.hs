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


type Solver a = StateT TypeSolution (Either SolvingError) a


type TypeSolution = Map T.TypeVariable T.Type


data SolvingError
    = UnsolvableConstraint T.Type T.Type
    | NotAFunction
    deriving (Eq, Show)


fail :: SolvingError -> Solver a
fail e =
    lift <| Left e


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


        Simple a b ->
            if a == b then
                return ()
            else
                fail <| UnsolvableConstraint a b


        IfThenElse { condition, whenTrue, whenFalse, returnType } -> do
            solveConstraint (Simple condition T.Bool)
            solveConstraint (Simple whenTrue whenFalse)
            solveConstraint (Simple returnType whenFalse)


        Application { functionReference, args, returnType } -> do
            (resultType, constraints) <-
                List.foldl
                    (\monad arg -> do
                        (currentType, constraints) <- monad
                        case currentType of
                            T.Function (T.FunctionType param resultingType) ->
                                return
                                    ( resultingType
                                    , constraints ++ [Simple arg param]
                                    )

                            _ ->
                                fail NotAFunction

                    )
                    (return (functionReference, []))
                    (NonEmpty.toList args)


            constraints ++ [Simple returnType resultType]
                |> traverse solveConstraint
                |> void



        Function { functionType, params, body } ->
            return ()


-- TODO - Make a function that takes care of solving simple constraint to use
-- in other constraint solution
