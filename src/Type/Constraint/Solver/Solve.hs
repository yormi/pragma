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
                Solver.mostPrecised (Constraint.quotedType condition)
            precisedWhenTrue <-
                Solver.mostPrecised (Constraint.quotedType whenTrue)
            precisedWhenFalse <-
                Solver.mostPrecised (Constraint.quotedType whenFalse)

            solveSimple
                (Constraint.quotedType condition)
                T.Bool
                (IfConditionMustBeABool
                    ((Constraint.codeQuote :: QuotedType -> CodeQuote)
                        condition
                    )
                    precisedCondition
                )
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


        Definition { dataId, type_, placeholder } ->
            Solver.NamedType dataId type_
                |> Solver.updateSolution placeholder


        Reference { reference, type_, placeholder } ->
            Solver.ReferenceType reference type_
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
    precisedA <- Solver.mostPrecised a
    precisedB <- Solver.mostPrecised b

    case (precisedA, precisedB) of
        (T.Function f, T.Function g) ->
            solveFunction f g error

        (T.Custom _ argsA, T.Custom _ argsB) ->
            List.zip argsA argsB
                |> traverse (\(argA, argB) -> solveSimple argA argB error)
                |> void

        -- TODO - Make sure this is correct
        (T.Placeholder pA, T.Placeholder pB) -> do
            if pA == pB then
                return ()
            else
                Solver.fail error

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
    a <- Solver.mostPrecised argA
    b <- Solver.mostPrecised argB

    solveSimple a b error
    solveSimple returnA returnB error
