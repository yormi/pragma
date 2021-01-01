module Type.Constraint.Solver.Model
    ( Solver
    , SolvingError(..)
    , Solution
    , SolutionType(..)
    , deducedSoFar
    , fail
    , nextPlaceholder
    , nextVariable
    , processSolution
    , updateSolution
    ) where

import Control.Monad.State (StateT)
import qualified Control.Monad.State as State
import qualified Data.Map as Map

import AST.CodeQuote (CodeQuote)
import AST.Identifier (ReferenceId, TypeVariableId)
import qualified AST.Identifier as Identifier
import AST.TypeAnnotation (TypeAnnotation)
import Type.Constraint.Reference (Reference)
import qualified Type.Model as T


data SolutionType
    = InstanceType T.Type
    | ReferenceType
        { reference :: Reference
        , type_ :: TypeAnnotation
        }
    deriving (Eq, Show)


type Solver a =
    StateT State (Either SolvingError) a


data State =
    State
        { solution :: Solution
        , nextTypePlaceholder :: T.TypePlaceholder
        , nextTypeVariable :: Int
        }


type Solution
    = Map T.TypePlaceholder SolutionType


initialState :: T.TypePlaceholder -> State
initialState nextAvailableTypeVariable =
    State
        { solution = Map.empty
        , nextTypePlaceholder = nextAvailableTypeVariable
        , nextTypeVariable = 0
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
        , signatureType :: TypeAnnotation
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
    let (T.TypePlaceholder next) = nextTypePlaceholder state
    State.put <| state { nextTypePlaceholder = T.TypePlaceholder <| next + 1 }
    next
        |> T.TypePlaceholder
        |> T.Placeholder
        |> return


nextVariable :: Solver TypeVariableId
nextVariable = do
    state <- State.get
    let next = nextTypeVariable state
    State.put <| state { nextTypeVariable = next + 1 }
    Identifier.generateTypeVariableId next
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
