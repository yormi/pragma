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
import qualified AST.Expression as E
import qualified Type as T
import qualified Utils.List as List
import qualified Utils.Maybe as Maybe


data SolutionType
    = InstanceType T.Type
    | NamedType [T.TypeVariable] T.Type
    deriving (Eq, Show)


type Solver a =
    StateT State (Either SolvingError) a


data State =
    State
        { solution :: Solution
        , nextTypeVariable :: T.TypeVariable
        }


type Solution
    = Map T.TypeVariable SolutionType


initialState :: T.TypeVariable -> State
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
        , functionName :: E.Identifier
        , functionType :: T.Type
        }
    | BadApplication
        { codeQuote :: CodeQuote
        , functionName :: E.Identifier
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
    let
        instantiate genericTypes t = do
            instances <- traverse (const freshVariable) genericTypes
            let replacements =
                    instances
                        |> List.zip genericTypes
                        |> Map.fromList
            replaceType replacements t
                |> return

        replaceType replacements t =
            case t of
                T.Variable variable ->
                    Map.lookup variable replacements
                        |> Maybe.withDefault t

                T.Function (T.FunctionType arg returning) ->
                    let
                        replacedArg =
                            replaceType replacements arg

                        replaceReturning =
                            replaceType replacements returning
                    in
                    T.FunctionType replacedArg replaceReturning
                        |> T.Function

                _ ->
                    t

    in
    case type_ of
        T.Variable v -> do
            state <- State.get
            let closest = Map.lookup v (solution state)

            case closest of
                Just (NamedType genericTypes t) ->
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


freshVariable :: Solver T.Type
freshVariable = do
    state <- State.get
    let next = nextTypeVariable state
    State.put <| state { nextTypeVariable = next + 1 }
    return <| T.Variable next


updateSolution :: T.TypeVariable -> SolutionType -> Solver ()
updateSolution typeVariable concludedType = do
    state <- State.get
    let typeSolution = solution state

    case Map.lookup typeVariable typeSolution of
        Nothing ->
            addToSolution typeVariable concludedType

        Just (InstanceType (T.Variable newTypeVariable)) ->
            updateSolution newTypeVariable concludedType

        Just a ->
            if a == concludedType then
                return ()
            else
                fail <| TypeVariableCannotSatisfyBothConstraint a concludedType


addToSolution :: T.TypeVariable -> SolutionType -> Solver ()
addToSolution v t =
    State.modify
        (\state ->
            solution state
                |> Map.insert v t
                |> (\newSolution -> state { solution = newSolution })
        )



processSolution :: T.TypeVariable -> Solver a -> Either SolvingError Solution
processSolution nextAvailableTypeVariable =
    flip State.execStateT (initialState nextAvailableTypeVariable)
        >> map solution
