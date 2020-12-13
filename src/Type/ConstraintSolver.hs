module Type.ConstraintSolver
    ( SolvingError(..)
    , TypeSolution
    , solve
    ) where

import Control.Monad.State (StateT)
import qualified Control.Monad.State as State
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map (Map)
import qualified Data.Map as Map

import AST.CodeQuote (CodeQuote)
import qualified AST.Expression as E
import qualified Type as T
import Type.Constraint.Model (Constraint(..), QuotedType(..))
import qualified Type.Constraint.Model as Constraint
import qualified Utils.Maybe as Maybe


type Solver a = StateT TypeSolution (Either SolvingError) a


type TypeSolution = Map T.TypeVariable T.Type


data SolvingError
    = TypeVariableCannotSatisfyBothConstraint T.Type T.Type
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
    | ShouldNotHappen String
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


        T.Function (T.FunctionType arg returnType) -> do
            precisedArg <- mostPrecised arg
            precisedReturnType <- mostPrecised returnType
            T.FunctionType precisedArg precisedReturnType
                |> T.Function
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
                fail <| TypeVariableCannotSatisfyBothConstraint a concludedType


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
        IfThenElse
            { codeQuote, condition, whenTrue, whenFalse, returnType }
            -> do
            solveSimple
                (Constraint.type_ condition)
                T.Bool
                (IfConditionMustBeABool
                    ((Constraint.codeQuote :: QuotedType -> CodeQuote)
                        condition
                    )
                    (Constraint.type_ condition)
                )
            solveSimple
                (Constraint.type_ whenTrue)
                (Constraint.type_ whenFalse)
                (BothIfAlternativesMustHaveSameType
                    codeQuote
                    (Constraint.type_ whenTrue)
                    (Constraint.type_ whenFalse)
                )
            solveSimple
                returnType
                (Constraint.type_ whenFalse)
                (ShouldNotHappen "Solving temporary return type to return type")


        Application
            { codeQuote
            , functionName
            , functionReference
            , argTypes
            , returnType
            } ->
            let
                functionType =
                    buildFunction (NonEmpty.toList argTypes) returnType
            in do
            referenceType <- mostPrecised functionReference
            case referenceType of
                T.Function _ ->
                    solveSimple
                        functionReference
                        functionType
                        (BadApplication
                            codeQuote
                            functionName
                            referenceType
                            functionType
                        )

                _ ->
                    NotAFunction codeQuote functionName referenceType
                        |> fail


        Function { codeQuote, signatureType, params, body } ->
            let
                definitionType =
                    buildFunction params body
            in do
            precisedFunctionType <- mostPrecised signatureType
            precisedActualType <- mostPrecised definitionType
            solveSimple
                signatureType
                definitionType
                (FunctionDefinitionMustMatchType
                    codeQuote
                    precisedFunctionType
                    precisedActualType
                )


buildFunction :: [T.Type] -> T.Type -> T.Type
buildFunction params finalType =
    List.foldl
        (\builtType p ->
            T.Function <| T.FunctionType p builtType
        )
        finalType
        (List.reverse params)


solveSimple :: T.Type -> T.Type -> SolvingError -> Solver ()
solveSimple a b error = do
    precisedA <- mostPrecised a
    precisedB <- mostPrecised b

    case (precisedA, precisedB) of
        (T.Function f, T.Function g) ->
            solveFunction f g error

        -- I'm skeptic about this... should not fail I think
        (T.Variable _, T.Variable _) -> do
            fail error

        (T.Variable variableA, _) -> do
            updateTypeSolution variableA b

        (_, T.Variable variableB) -> do
            updateTypeSolution variableB a

        _ ->
            if precisedA == precisedB then
                return ()
            else
                fail error


solveFunction :: T.FunctionType -> T.FunctionType -> SolvingError -> Solver ()
solveFunction (T.FunctionType argA returnA) (T.FunctionType argB returnB) error = do
    a <- mostPrecised argA
    b <- mostPrecised argB

    solveSimple a b error
    solveSimple returnA returnB error
