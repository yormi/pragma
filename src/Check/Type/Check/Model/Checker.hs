module Check.Type.Check.Model.Checker
    ( Checker
    , Error(..)
    , SolutionType(..)
    , fail
    , updateSolution
    , updateInstanceSolution
    , linkSolution
    , nextInstance
    , run
    ) where

import Control.Monad.State (StateT)
import qualified Control.Monad.State as State

import qualified Check.Type.Arrange as A
import qualified Check.Type.Check.Model.GenericType as Generic
import Check.Type.Check.Model.Error (Error(..))
import Check.Type.Check.Model.InstancedType
    ( InstanceId
    , InstancedType(..)
    , UnboundId
    )
import Check.Type.Check.Model.Solution
    ( Solution
    , SolutionType
    )
import qualified Check.Type.Check.Model.Solution as Solution
import qualified Utils.Map as Map


type Checker a =
    StateT State (Either Error) a


data State =
    State
        { solution :: Solution
        , instanceSolution :: InstanceSolution
        , nextUnbound :: UnboundId
        , nextGenericVariable :: Generic.VariableId
        , nextInstanceId :: InstanceId
        }


initialSolution :: Solution
initialSolution =
    Map.empty


type InstanceSolution
    = Map InstanceId SolutionType


initialInstanceSolution :: InstanceSolution
initialInstanceSolution =
    Map.empty


deducedSoFar :: Checker Solution
deducedSoFar =
    State.get
        |> map solution


nextInstance :: Checker InstancedType
nextInstance = do
    state <- State.get
    let next = nextInstanceId state
    State.put <| state { nextInstanceId = next + 1 }
    next
        |> Instance
        |> return


updateSolution :: A.Link -> SolutionType -> Checker ()
updateSolution link concludedType = do
    state <- State.get
    let typeSolution = solution state

    case Map.lookup link typeSolution of
        Nothing ->
            addToSolution link concludedType

        Just (Solution.Instanced (Instance instanceId)) ->
            updateInstanceSolution instanceId concludedType

        Just a ->
            if a == concludedType then
                return ()
            else
                TypeVariableCannotSatisfyBothConstraint a
                    concludedType
                    typeSolution
                    |> fail


updateInstanceSolution :: InstanceId -> SolutionType -> Checker ()
updateInstanceSolution instancedId concludedType =
    return ()


addToSolution :: A.Link -> SolutionType -> Checker ()
addToSolution link t =
    State.modify
        (\state ->
            solution state
                |> Map.insert link t
                |> (\newSolution -> state { solution = newSolution })
        )


linkSolution :: A.Link -> Checker SolutionType
linkSolution link = do
    state <- State.get
    let maybeSolution =
            state
                |> solution
                |> Map.lookup link

    case maybeSolution of
        Just s ->
            return s

        Nothing ->
            ThisIsABug "The link should always be resolved in a correct order"
                |> fail


fail :: Error -> Checker a
fail =
    lift << Left


run :: Checker a -> Either Error a
run checker =
    let
        initialState =
            State initialSolution initialInstanceSolution 0 0 0
    in
    State.evalStateT checker initialState
