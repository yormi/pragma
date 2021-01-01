module Type.Constraint.Solver.Solve
    ( SolvingError(..)
    , Solution
    , solve
    ) where

import qualified Data.List.NonEmpty as NonEmpty

import AST.CodeQuote (CodeQuote)
import qualified Type.Model as T
import Type.Constraint.Model (Constraint(..), QuotedType(..))
import qualified Type.Constraint.Model as Constraint
import Type.Constraint.Solver.Model (Solver, SolvingError(..), Solution)
import qualified Type.Constraint.Solver.Model as Solver
import qualified Type.Constraint.Solver.Solution as Solution
import qualified Type.Constraint.Solver.TypeAnnotation as TypeAnnotation
import qualified Utils.List as List


solve :: T.TypePlaceholder -> [Constraint] -> Either SolvingError Solution
solve nextAvailableTypeVariable constraints =
    traverse solveConstraint constraints
        |> Solver.processSolution nextAvailableTypeVariable


solveConstraint :: Constraint -> Solver ()
solveConstraint constraint =
    case constraint of
        IfThenElse
            { codeQuote, condition, whenTrue, whenFalse, placeholder }
            -> do
            precisedCondition <-
                Solution.mostPrecised (Constraint.quotedType condition)

            solveSimple
                (Constraint.quotedType condition)
                T.Bool
                (IfConditionMustBeABool
                    ((Constraint.codeQuote :: QuotedType -> CodeQuote)
                        condition
                    )
                    precisedCondition
                )

            precisedWhenTrue <-
                Solution.mostPrecised (Constraint.quotedType whenTrue)
            precisedWhenFalse <-
                Solution.mostPrecised (Constraint.quotedType whenFalse)

            solveSimple
                (Constraint.quotedType whenTrue)
                (Constraint.quotedType whenFalse)
                (BothIfAlternativesMustHaveSameType
                    codeQuote
                    precisedWhenTrue
                    precisedWhenFalse
                )

            Constraint.quotedType whenFalse
                |> Solver.InstanceType
                |> Solver.updateSolution placeholder


        Application
            { codeQuote
            , functionName
            , functionReference
            , argTypes
            , placeholder
            } ->
            let
                functionType =
                    buildFunction
                        (NonEmpty.toList argTypes)
                        (T.Placeholder placeholder)
            in do
            referenceType <- Solution.mostPrecised functionReference
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


        Function
            { codeQuote, signatureType, params, body } ->
            let
                definition =
                    buildFunction (map T.Placeholder params) body

            in do
            precisedDefinition <- Solution.mostPrecised definition
            solution <- Solver.deducedSoFar

            if TypeAnnotation.isTypeMatching signatureType precisedDefinition then
                return ()
            else
                FunctionDefinitionMustMatchType
                    codeQuote
                    signatureType
                    precisedDefinition
                    solution
                    |> Solver.fail


        TopLevelDefinition { reference, typeAnnotation, placeholder } -> do
            Solver.ReferenceType reference typeAnnotation
                |> Solver.updateSolution placeholder


        LetDefinition { reference, type_, placeholder } -> do
            typeAnnotation <- TypeAnnotation.fromType type_
            Solver.ReferenceType reference typeAnnotation
                |> Solver.updateSolution placeholder


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
    precisedA <- Solution.mostPrecised a
    precisedB <- Solution.mostPrecised b

    case (precisedA, precisedB) of
        (T.Function f, T.Function g) ->
            solveFunction f g error

        (T.Custom _ argsA, T.Custom _ argsB) -> do
            List.zip argsA argsB
                |> traverse (\(argA, argB) -> solveSimple argA argB error)
                |> void


        (T.Placeholder pA, T.Placeholder pB) -> do
            if pA == pB then
                return ()
            else do
                p <- Solver.nextPlaceholder
                Solver.updateSolution pA (Solver.InstanceType p)
                Solver.updateSolution pB (Solver.InstanceType p)
                return ()

        (T.Placeholder pA, _) -> do
            Solver.updateSolution pA (Solver.InstanceType b)

        (_, T.Placeholder pB) -> do
            Solver.updateSolution pB (Solver.InstanceType a)

        _ ->
            if precisedA == precisedB then
                return ()
            else
                Solver.fail error


solveFunction :: T.FunctionType -> T.FunctionType -> SolvingError -> Solver ()
solveFunction (T.FunctionType argA returnA) (T.FunctionType argB returnB) error = do
    a <- Solution.mostPrecised argA
    b <- Solution.mostPrecised argB

    solveSimple a b error
    solveSimple returnA returnB error
