module Type.Constraint.Solver.Entry
    ( SolvingError(..)
    , Solution
    , solve
    ) where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Set as Set

import AST.CodeQuote (CodeQuote)
import qualified Type.Model as T
import Type.Constraint.Model (Constraint(..), QuotedType(..))
import qualified Type.Constraint.Model as Constraint
import qualified Type.Constraint.Solver.Deduce as Deduce
import qualified Type.Constraint.Solver.Instantiate as Instantiate
import Type.Constraint.Solver.Model.Instanced (InstancedType)
import qualified Type.Constraint.Solver.Model.Instanced as I
import qualified Type.Constraint.Solver.Generalize as Generalize
import Type.Constraint.Solver.Model.Solution (Solution)
import qualified Type.Constraint.Solver.Model.Solution as Solution
import Type.Constraint.Solver.Model.Solver (Solver, SolvingError(..))
import qualified Type.Constraint.Solver.Model.Solver as Solver
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
                Constraint.quotedType condition
                    |> deducedSoFar

            solveSimple
                precisedCondition
                I.Bool
                (IfConditionMustBeABool
                    ((Constraint.codeQuote :: QuotedType -> CodeQuote)
                        condition
                    )
                    precisedCondition
                )

            precisedWhenTrue <-
                Constraint.quotedType whenTrue
                    |> deducedSoFar
            precisedWhenFalse <-
                Constraint.quotedType whenFalse
                    |> deducedSoFar
            solveSimple
                precisedWhenTrue
                precisedWhenFalse
                (BothIfAlternativesMustHaveSameType
                    codeQuote
                    precisedWhenTrue
                    precisedWhenFalse
                )

            lastDeduction <-
                Constraint.quotedType whenFalse
                    |> deducedSoFar
            lastDeduction
                |> Solution.Instanced
                |> Solver.updateSolution placeholder


        Application
            { codeQuote
            , functionName
            , functionReference
            , argTypes
            , placeholder
            } -> do
            functionType <-
                buildFunction
                    (NonEmpty.toList argTypes)
                    (T.Placeholder placeholder)
            referenceType <- deducedSoFar functionReference
            solveSimple
                referenceType
                functionType
                (BadApplication
                    codeQuote
                    functionName
                    referenceType
                    functionType
                )


        Function { codeQuote, signatureType, params, body } -> do
            definition <- buildFunction params body
            instancedSignature <- Instantiate.fromTypeAnnotation signatureType

            solveSimple
                instancedSignature
                definition
                (FunctionDefinitionMustMatchType
                    codeQuote
                    signatureType
                    definition
                )


        TopLevelDefinition { reference, typeAnnotation, placeholder } -> do
            let generalized = Generalize.fromTypeAnnotation typeAnnotation
            Solution.Generic reference generalized
                |> Solver.updateSolution placeholder


        LetDefinition { reference, type_, placeholder, generated } -> do
            (instantiationPlaceholders, precised) <-
                deducedSoFar type_
                    |> Solver.recordingGeneratedPlaceholder
            let expressionPlaceholders =
                    Set.union instantiationPlaceholders generated
            genericType <- Generalize.fromDefinition expressionPlaceholders precised
            Solution.Generic reference genericType
                |> Solver.updateSolution placeholder


deducedSoFar :: T.Type -> Solver InstancedType
deducedSoFar =
    I.fromType >> Deduce.mostPrecised


buildFunction :: [T.Type] -> T.Type -> Solver InstancedType
buildFunction params finalType =
    List.foldl
        (\builtType p ->
            T.Function <| T.FunctionType p builtType
        )
        finalType
        (List.reverse params)
        |> deducedSoFar


solveSimple
    :: InstancedType -> InstancedType -> (Solution -> SolvingError) -> Solver ()
solveSimple a b error = do
    precisedA <- Deduce.mostPrecised a
    precisedB <- Deduce.mostPrecised b

    case (precisedA, precisedB) of
        (I.Function argA returnA, I.Function argB returnB) -> do
            precisedArgA <- Deduce.mostPrecised argA
            precisedArgB <- Deduce.mostPrecised argB

            solveSimple precisedArgA precisedArgB error
            solveSimple returnA returnB error


        (I.Custom _ argsA, I.Custom _ argsB) -> do
            List.zip argsA argsB
                |> traverse (\(argA, argB) -> solveSimple argA argB error)
                |> void


        (I.Placeholder pA, I.Placeholder pB) -> do
            if pA == pB then
                return ()
            else do
                p <- Solver.nextPlaceholder
                Solver.updateSolution pA (Solution.Instanced p)
                Solver.updateSolution pB (Solution.Instanced p)
                return ()


        (I.Placeholder pA, _) -> do
            Solver.updateSolution pA (Solution.Instanced b)


        (_, I.Placeholder pB) -> do
            Solver.updateSolution pB (Solution.Instanced a)


        _ ->
            if precisedA == precisedB then
                return ()
            else
                Solver.fail error
