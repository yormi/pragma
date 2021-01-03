module Type.Constraint.Solver.Solve
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
import Type.Constraint.Solver.Instanced (InstancedType)
import qualified Type.Constraint.Solver.Instanced as I
import qualified Type.Constraint.Solver.Generic as G
import Type.Constraint.Solver.Model (Solver, SolvingError(..), Solution)
import qualified Type.Constraint.Solver.Model as Solver
import qualified Type.Constraint.Solver.Solution as Solution
import qualified Type.Constraint.Solver.TypeAnnotation as TypeAnnotation
import qualified Utils.List as List

import qualified Printer.Type.Generic as GenericPrinter
import qualified Printer.Type.Solution as SolutionPrinter
import qualified Utils.String as String


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
                |> Solver.Instanced
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
            case referenceType of
                I.Placeholder _ ->
                    solveSimple
                        referenceType
                        functionType
                        (BadApplication
                            codeQuote
                            functionName
                            referenceType
                            functionType
                        )

                I.Function _ _ ->
                    solveSimple
                        referenceType
                        functionType
                        (BadApplication
                            codeQuote
                            functionName
                            referenceType
                            functionType
                        )

                _ -> do
                    solution <- Solver.deducedSoFar
                    NotAFunction codeQuote functionName referenceType solution
                        |> Solver.fail


        Function { codeQuote, signatureType, params, body } -> do
            definition <-
                buildFunction (map T.Placeholder params) body
            precisedDefinition <- Solution.mostPrecised definition

            instancedSignature <-
                signatureType
                    |> TypeAnnotation.instantiate

            solveSimple
                instancedSignature
                precisedDefinition
                (FunctionDefinitionMustMatchType
                    codeQuote
                    signatureType
                    precisedDefinition
                )


        TopLevelDefinition { reference, typeAnnotation, placeholder } -> do
            let generalized = G.fromTypeAnnotation typeAnnotation
            Solver.Generic reference generalized
                |> Solver.updateSolution placeholder


        LetDefinition { reference, type_, placeholder, generated } -> do
            solution <- Solver.deducedSoFar
            (instantiationPlaceholders, precised) <-
                deducedSoFar type_
                    |> Solver.recordingGeneratedPlaceholder
                    |> map (pipeTrace
                        ([ "before    " ++ show reference ++ "    "
                            ++ show type_
                        , "deduced   " ++ show reference ++ "    "
                        ]
                            |> String.mergeLines
                        )
                    )
            genericType <- Solution.generalize (Set.union instantiationPlaceholders generated) precised
                    |> map (pipeTrace <|
                        ([ "solution:"
                        , SolutionPrinter.print solution
                        , "generalize   " ++ show reference ++ "    "
                        ]
                            |> String.mergeLines
                        )
                        )
            Solver.Generic reference genericType
                |> Solver.updateSolution placeholder


deducedSoFar :: T.Type -> Solver InstancedType
deducedSoFar =
    I.fromType >> Solution.mostPrecised


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
    precisedA <- Solution.mostPrecised a
    precisedB <- Solution.mostPrecised b

    case (precisedA, precisedB) of
        (I.Function argA returnA, I.Function argB returnB) -> do
            precisedArgA <- Solution.mostPrecised argA
            precisedArgB <- Solution.mostPrecised argB

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
                Solver.updateSolution pA (Solver.Instanced p)
                Solver.updateSolution pB (Solver.Instanced p)
                return ()
                    |> trace ("Solving Placeholders " ++ show p)


        (I.Placeholder pA, _) -> do
            Solver.updateSolution pA (Solver.Instanced b)


        (_, I.Placeholder pB) -> do
            Solver.updateSolution pB (Solver.Instanced a)


        _ ->
            if precisedA == precisedB then
                return ()
            else do
                solution <- Solver.deducedSoFar
                Solver.fail (error solution)
