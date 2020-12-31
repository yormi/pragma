module Type.Constraint.Solver.Model
    ( Solver
    , SolvingError(..)
    , Solution
    , SolutionType(..)
    , deducedSoFar
    , fail
    , nextPlaceholder
    , processSolution
    , updateSolution
    ) where

import Control.Monad.State (StateT)
import qualified Control.Monad.State as State
import qualified Data.Map as Map

import AST.CodeQuote (CodeQuote)
import AST.Identifier (ReferenceId)
import Type.Constraint.Reference (Reference)
import qualified Type.Model as T


data SolutionType
    = InstanceType T.Type
    | ReferenceType
        { reference :: Reference
        , type_ :: T.Type
        }
    deriving (Eq, Show)


type Solver a =
    StateT State (Either SolvingError) a


data State =
    State
        { solution :: Solution
        , nextTypeVariable :: T.TypePlaceholder
        }


type Solution
    = Map T.TypePlaceholder SolutionType


initialState :: T.TypePlaceholder -> State
initialState nextAvailableTypeVariable =
    State
        { solution = Map.empty
        , nextTypeVariable = nextAvailableTypeVariable
        }


data SolvingError
    = TypeVariableCannotSatisfyBothConstraint SolutionType SolutionType
    | IfConditionMustBeABool
        { codeQuote :: CodeQuote
        , type_ :: T.Type
        }
    | BothIfAlternativesMustHaveSameType
        { codeQuote :: CodeQuote
        , whenTrue :: T.Type
        , whenFalse :: T.Type
        }
    | NotAFunction
        { codeQuote :: CodeQuote
        , functionName :: ReferenceId
        , functionType :: T.Type
        }
    | BadApplication
        { codeQuote :: CodeQuote
        , functionName :: ReferenceId
        , referenceType :: T.Type
        , functionType :: T.Type
        }
    | FunctionDefinitionMustMatchType
        { codeQuote :: CodeQuote
        , signatureType :: T.Type
        , definitionType :: T.Type
        , solutionSoFar :: Solution
        }
    | TODO String
    | ShouldNotHappen String
    deriving (Eq, Show)


deducedSoFar :: Solver Solution
deducedSoFar =
    State.get
        |> map solution


fail :: SolvingError -> Solver a
fail e =
    lift <| Left e


nextPlaceholder :: Solver T.Type
nextPlaceholder = do
    state <- State.get
    let (T.TypePlaceholder next) = nextTypeVariable state
    State.put <| state { nextTypeVariable = T.TypePlaceholder <| next + 1 }
    next
        |> T.TypePlaceholder
        |> T.Placeholder
        |> return

updateSolution :: T.TypePlaceholder -> SolutionType -> Solver ()
updateSolution placeholder concludedType = do
    state <- State.get
    let typeSolution = solution state

    case Map.lookup placeholder typeSolution of
        Nothing ->
            addToSolution placeholder concludedType

        Just (InstanceType (T.Placeholder newPlaceholder)) ->
            updateSolution newPlaceholder concludedType

        Just a ->
            if a == concludedType then
                return ()
            else
                fail <| TypeVariableCannotSatisfyBothConstraint a concludedType


addToSolution :: T.TypePlaceholder -> SolutionType -> Solver ()
addToSolution placeholder t =
    State.modify
        (\state ->
            solution state
                |> Map.insert placeholder t
                |> (\newSolution -> state { solution = newSolution })
        )


processSolution :: T.TypePlaceholder -> Solver a -> Either SolvingError Solution
processSolution nextAvailableTypeVariable =
    flip State.execStateT (initialState nextAvailableTypeVariable)
        >> map solution
