module Type.Constraint.Solver.Model
    ( Solver
    , SolvingError(..)
    , Solution
    , SolutionType(..)
    , fail
    , mostPrecised
    , processSolution
    , updateSolution
    ) where

import Control.Monad.State (StateT)
import qualified Control.Monad.State as State
import Data.Map (Map)
import qualified Data.Map as Map

import AST.CodeQuote (CodeQuote)
import AST.Identifier (ConstructorId, DataId, ReferenceId, TypeVariableId)
import qualified Type.Model as T
import qualified Utils.List as List
import qualified Utils.Maybe as Maybe


data SolutionType
    = InstanceType T.Type
    | NamedType
        { identifier :: DataId
        , unconstrainedTypeVariables :: [TypeVariableId]
        , type_ :: T.Type
        }
    | ConstructorType
        { constructorId :: ConstructorId
        , unconstrainedTypeVariables :: [TypeVariableId]
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
        }
    | TODO String
    | ShouldNotHappen String
    deriving (Eq, Show)


fail :: SolvingError -> Solver a
fail e =
    lift <| Left e


mostPrecised :: T.Type -> Solver T.Type
mostPrecised type_ =
    case type_ of
        T.Placeholder p -> do
            state <- State.get
            let morePrecise = Map.lookup p (solution state)

            case morePrecise of
                Just (NamedType _ genericTypes t) ->
                    instantiate genericTypes t

                Just (InstanceType t) ->
                    mostPrecised t

                Nothing ->
                    return type_


        T.Function (T.FunctionType arg returnType) -> do
            precisedArg <- mostPrecised arg
            precisedReturnType <- mostPrecised returnType
            T.FunctionType precisedArg precisedReturnType
                |> T.Function
                |> return


        _ ->
            return type_


instantiate :: [TypeVariableId] -> T.Type -> Solver T.Type
instantiate typeVariables generalizedType =
    let
        replaceVariables replacements type_ =
            case type_ of
                T.Variable variable ->
                    Map.lookup variable replacements
                        |> Maybe.withDefault type_

                T.Function (T.FunctionType arg returning) ->
                    let
                        replacedArg =
                            replaceVariables replacements arg

                        replaceReturning =
                            replaceVariables replacements returning
                    in
                    T.FunctionType replacedArg replaceReturning
                        |> T.Function

                _ ->
                    type_

    in do
    instances <- traverse (const freshVariable) typeVariables
    let replacements =
            instances
                |> List.zip typeVariables
                |> Map.fromList
    replaceVariables replacements generalizedType
        |> return


freshVariable :: Solver T.Type
freshVariable = do
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
