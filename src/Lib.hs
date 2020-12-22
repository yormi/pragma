module Lib (run) where

import qualified Data.Bifunctor as Bifunctor
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.String as String
import qualified System.Directory as Directory
import System.IO (readFile)

import qualified AST.Module as M
import qualified AST.TypeAnnotation as TypeAnnotation
import qualified Generator
import qualified Parser.Parser as Parser
import qualified Parser.Module as Module
import qualified Printer as TypePrinter
import qualified Printer.Type.Constraint as ConstraintPrinter
import qualified Printer.Type.SolverError as SolverErrorPrinter
import Type.Constraint.Solver.Model (Solution)
import qualified Type.Constraint.Solver.Solve as ConstraintSolver
import qualified Type.Constraint.Gatherer.Model as Gatherer
import Type.Constraint.Model (Constraint)
import qualified Type.Constraint.Model as Constraint
import qualified Type.Constraint.Gatherer.Module as Module
import qualified Type as T
import qualified Utils.Either as Either
import qualified Utils.List as List
import qualified Utils.String as String


import qualified Text.Layout.Table as Table


run :: IO ()
run = do
    dir <- Directory.getCurrentDirectory
    putStrLn dir

    let filePath = "test/test"
    let parser = Module.moduleParser

    file <- readFile filePath
    putStrLn file

    putStrLn <| "\n\n--- PARSING ---\n"
    let parsedModule =
            Parser.runParser parser filePath file
                |> Either.mapLeft show
                |> Either.mapLeft (\s -> "PARSING ERROR: " ++ s)


    --putStrLn <| Either.fold show TypePrinter.printModule parsedModule


    putStrLn <| "\n\n--- TYPE CHECK ---\n"


    let constraintResults =
            (parsedModule
                |> traverse constraintGathering
                |> map join
            ) :: [Either String [Constraint]]

    let solverResult =
            (constraintResults
                |> map
                    (bind
                        (ConstraintSolver.solve 100
                            >> Either.mapLeft
                                (SolverErrorPrinter.printSolvingError file)
                            >> Either.mapLeft
                                (\s -> "CONSTRAINT SOLVING ERROR: \n" ++ s)
                        )
                    )
            ) :: [Either String Solution]


    putStrLn <| "\n\n--- TYPE SOLUTION ---\n"
    List.zip constraintResults solverResult
        |> traverse
            (\(gathering, solving) -> do
                printConstraintResults gathering
                putStrLn "\nSolution :"
                printSolutionResult solving
                putStrLn "\n---\n"
            )
        |> void


    case sequence solverResult of
        Right _ -> do
            putStrLn <| "\n\n--- CODE GENERATION ---\n"
            parsedModule
                |> map
                    (\m ->
                        Table.colsAllG Table.center
                            [ String.splitLines <| Generator.generate m
                            , String.splitLines <| TypePrinter.printModule m
                            ]
                    )
                |> map List.singleton
                |> traverse
                    ( Table.tableString
                        [ Table.fixedCol 80 Table.left
                        , Table.column
                            (Table.fixedUntil 80)
                            Table.left
                            Table.noAlign
                            (Table.singleCutMark "...")
                        ]
                        Table.unicodeRoundS
                        (Table.titlesH ["Generated NodeJS", "Pragma Source"])
                        >> putStrLn
                    )
                |> void


        _ ->
            return ()


constraintGathering :: M.Module -> [Either String [Constraint]]
constraintGathering parsedModule@(M.Module topLevels) =
    map
        (gatherConstraints parsedModule
            >> Either.mapLeft show
            >> Either.mapLeft (\s -> "CONSTRAINT GATHERING ERROR: " ++ s)
        )
        topLevels


printConstraintResults :: Either String [Constraint] -> IO ()
printConstraintResults result =
    case result of
        Right constraints ->
            constraints
                |> map (ConstraintPrinter.printConstraint)
                |> String.unlines
                |> putStrLn

        Left e ->
            putStrLn e


printSolutionResult :: Either String Solution -> IO ()
printSolutionResult result =
    case result of
        Right solution ->
            printSolution solution

        Left e ->
            putStrLn e


printSolution :: Solution -> IO ()
printSolution solution =
    let
        solutions =
            solution
                |> Map.toList
                |> map (Bifunctor.first (\n -> "a" ++ show n))
                |> map
                    (\(variable, type_) ->
                        variable
                            ++ " :: "
                            ++ TypePrinter.printTypeSolution type_
                    )
                |> List.intercalate "\n\t"
    in
    putStrLn <| "\t" ++ solutions


gatherConstraints
    :: M.Module -> M.TopLevel -> Either Gatherer.ConstraintError [Constraint]
gatherConstraints (M.Module topLevels) topLevel =
    (do
        context <-
            traverse
                (\t ->
                    case t of
                        M.Function { M.functionName, M.typeAnnotation } ->
                            let
                                hasTypeVariable =
                                    TypeAnnotation.extractTypeVariables
                                        typeAnnotation
                                        |> \ts -> List.length ts > 0

                            in do
                            signatureType <-
                                Module.signatureType typeAnnotation

                            if hasTypeVariable then do
                                returnType <- Gatherer.freshVariable
                                Constraint.Generalized signatureType returnType
                                    |> Gatherer.addConstraint

                                return (functionName, T.Variable returnType)
                            else
                                return (functionName, signatureType)

                )
                topLevels

        Module.gather topLevel
            |> Gatherer.withDataReferences context
    )
        |> Gatherer.gatherConstraints
