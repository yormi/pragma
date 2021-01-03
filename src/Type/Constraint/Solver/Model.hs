module Type.Constraint.Solver.Model
    ( module X
    , InstancedType(..)
    , Solver
    , SolvingError(..)
    , deducedSoFar
    , fail
    , nextInstance
    , nextPlaceholder
    , nextVariable
    , processSolution
    , recordingGeneratedPlaceholder
    , updateSolution
    ) where

import Control.Monad.State (StateT)
import qualified Control.Monad.State as State
import qualified Data.Map as Map
import qualified Data.Set as Set

import AST.CodeQuote (CodeQuote)
import AST.Identifier (ReferenceId, TypeVariableId)
import qualified AST.Identifier as Identifier
import AST.TypeAnnotation (TypeAnnotation)
import Type.Constraint.Solver.Instanced (InstanceId(..), InstancedType(..))
import qualified Type.Constraint.Solver.Instanced as I
import Type.Constraint.Solver.SolutionModel as X (Solution, SolutionType(..))
import qualified Type.Model as T
import qualified Utils.List as List

import qualified Printer.Type.Solution as SolutionPrinter


type Solver a =
    StateT State (Either SolvingError) a


data State =
    State
        { solution :: Solution
        , nextTypePlaceholder :: T.TypePlaceholder
        , nextTypeVariable :: Int
        , nextInstanceId :: InstanceId
        }


data SolvingError
    = TypeVariableCannotSatisfyBothConstraint SolutionType SolutionType
    | IfConditionMustBeABool
        { codeQuote :: CodeQuote
        , type_ :: InstancedType
        , solutionSoFar :: Solution
        }
    | BothIfAlternativesMustHaveSameType
        { codeQuote :: CodeQuote
        , whenTrue :: InstancedType
        , whenFalse :: InstancedType
        , solutionSoFar :: Solution
        }
    | NotAFunction
        { codeQuote :: CodeQuote
        , functionName :: ReferenceId
        , functionType :: InstancedType
        , solutionSoFar :: Solution
        }
    | BadApplication
        { codeQuote :: CodeQuote
        , functionName :: ReferenceId
        , referenceType :: InstancedType
        , functionType :: InstancedType
        , solutionSoFar :: Solution
        }
    | FunctionDefinitionMustMatchType
        { codeQuote :: CodeQuote
        , signatureType :: TypeAnnotation
        , definitionType :: InstancedType
        , solutionSoFar :: Solution
        }
    | TODO String
    | ShouldNotHappen String
    deriving (Eq, Show)


initialState :: T.TypePlaceholder -> State
initialState nextAvailableTypeVariable =
    State
        { solution = Map.empty
        , nextTypePlaceholder = nextAvailableTypeVariable
        , nextTypeVariable = 0
        , nextInstanceId = InstanceId 0
        }


deducedSoFar :: Solver Solution
deducedSoFar =
    State.get
        |> map solution


fail :: SolvingError -> Solver a
fail e =
    lift <| Left e


nextInstance :: Solver InstancedType
nextInstance = do
    state <- State.get
    let (InstanceId next) = nextInstanceId state
    State.put <| state { nextInstanceId = InstanceId <| next + 1 }
    InstanceId next
        |> I.Instance
        |> return


nextPlaceholder :: Solver InstancedType
nextPlaceholder = do
    state <- State.get
    let (T.TypePlaceholder next) = nextTypePlaceholder state
    State.put <| state { nextTypePlaceholder = T.TypePlaceholder <| next + 1 }
    next
        |> T.TypePlaceholder
        |> Placeholder
        |> return


recordingGeneratedPlaceholder :: Solver a -> Solver (Set T.TypePlaceholder, a)
recordingGeneratedPlaceholder solver =
    let
        readNextPlaceholderNumber = do
            state <- State.get
            let (T.TypePlaceholder placeholderNumber) =
                    nextTypePlaceholder state
            return placeholderNumber
    in do
    fromPlaceholder <- readNextPlaceholderNumber
    result <- solver
    nextPlaceholderAfter <- readNextPlaceholderNumber

    let generated =
            List.range fromPlaceholder nextPlaceholderAfter
                |> map T.TypePlaceholder
                |> Set.fromList
    return (generated, result)


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

        Just (Instanced (I.Placeholder newPlaceholder)) ->
            updateSolution newPlaceholder concludedType

        Just a ->
            if a == concludedType then
                return ()
            else
                fail <| TypeVariableCannotSatisfyBothConstraint a concludedType


addToSolution :: T.TypePlaceholder -> SolutionType -> Solver ()
addToSolution placeholder@(T.TypePlaceholder p) t =
    trace ("\t\tp" ++ show p ++ "\t=\t\t" ++ SolutionPrinter.printSolutionType t) <|
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
