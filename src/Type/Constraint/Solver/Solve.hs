module Type.Constraint.Solver.Solve
    ( SolvingError(..)
    , Solution
    , solve
    ) where

import qualified Data.List.NonEmpty as NonEmpty

import AST.CodeQuote (CodeQuote)
import qualified Type as T
import Type.Constraint.Model (Constraint(..), QuotedType(..))
import qualified Type.Constraint.Model as Constraint
import Type.Constraint.Solver.Model (Solver, SolvingError(..), Solution)
import qualified Type.Constraint.Solver.Model as Solver
import qualified Utils.List as List


solve :: T.TypeVariable -> [Constraint] -> Either SolvingError Solution
solve nextAvailableTypeVariable constraints =
    traverse solveConstraint constraints
        |> Solver.processSolution nextAvailableTypeVariable


solveConstraint :: Constraint -> Solver ()
solveConstraint constraint =
    case constraint of
        IfThenElse
            { codeQuote, condition, whenTrue, whenFalse, returnType }
            -> do
            precisedCondition <-
                Solver.mostPrecised (Constraint.type_ condition)
            precisedWhenTrue <-
                Solver.mostPrecised (Constraint.type_ whenTrue)
            precisedWhenFalse <-
                Solver.mostPrecised (Constraint.type_ whenFalse)

            solveSimple
                (Constraint.type_ condition)
                T.Bool
                (IfConditionMustBeABool
                    ((Constraint.codeQuote :: QuotedType -> CodeQuote)
                        condition
                    )
                    precisedCondition
                )
            solveSimple
                (Constraint.type_ whenTrue)
                (Constraint.type_ whenFalse)
                (BothIfAlternativesMustHaveSameType
                    codeQuote
                    precisedWhenTrue
                    precisedWhenFalse
                )

            Constraint.type_ whenFalse
                |> Solver.InstanceType
                |> Solver.updateSolution returnType


        Application
            { codeQuote
            , functionName
            , functionReference
            , argTypes
            , returnType
            } ->
            let
                functionType =
                    buildFunction
                        (NonEmpty.toList argTypes)
                        (T.Variable returnType)
            in do
            referenceType <- Solver.mostPrecised functionReference
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
                        |> Solver.fail


        Function { codeQuote, signatureType, params, body } ->
            let
                definitionType =
                    buildFunction params body
            in do
            precisedFunctionType <- Solver.mostPrecised signatureType
            precisedActualType <- Solver.mostPrecised definitionType
            solveSimple
                signatureType
                definitionType
                (FunctionDefinitionMustMatchType
                    codeQuote
                    precisedFunctionType
                    precisedActualType
                )

        Generalized { actualType, returnType } ->
            let
                generalize t = do
                    precised <- Solver.mostPrecised t
                    case precised of
                        T.Variable typeVariable -> do
                            return [typeVariable]

                        T.Function (T.FunctionType arg returningType) -> do
                            argVariables <- generalize arg
                            returnVariables <- generalize returningType

                            let typeVariables =
                                    argVariables ++ returnVariables
                                        |> List.unique
                            return typeVariables

                        _ ->
                            return []
            in do
            genericVariables <- generalize actualType
            Solver.updateSolution returnType
                (Solver.NamedType genericVariables actualType)


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
    precisedA <- Solver.mostPrecised a
    precisedB <- Solver.mostPrecised b

    case (precisedA, precisedB) of
        (T.Function f, T.Function g) ->
            solveFunction f g error

        -- TODO - Make sure this is correct
        (T.Variable variableA, T.Variable variableB) -> do
            if variableA == variableB then
                return ()
            else
                Solver.fail error
            -- <!> COMMENTED CURRENTLY INFINITE LOOPING <!>
            -- Solver.updateSolution variableA (Solver.InstanceType b)
            -- Solver.updateSolution variableB (Solver.InstanceType a)

        (T.Variable variableA, _) -> do
            Solver.updateSolution variableA (Solver.InstanceType b)

        (_, T.Variable variableB) -> do
            Solver.updateSolution variableB (Solver.InstanceType a)

        _ ->
            if precisedA == precisedB then
                return ()
            else
                Solver.fail error


solveFunction :: T.FunctionType -> T.FunctionType -> SolvingError -> Solver ()
solveFunction (T.FunctionType argA returnA) (T.FunctionType argB returnB) error = do
    a <- Solver.mostPrecised argA
    b <- Solver.mostPrecised argB

    solveSimple a b error
    solveSimple returnA returnB error
