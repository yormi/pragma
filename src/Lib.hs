module Lib (run) where

import qualified Data.Bifunctor as Bifunctor
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.String as String
import qualified System.Directory as Directory
import System.IO (readFile)

import qualified AST.Module as M
import qualified Generator
import qualified Parser.Parser as Parser
import qualified Parser.Module as Module
import qualified Printer as TypePrinter
import Type.ConstraintSolver (Solution)
import qualified Type.ConstraintSolver as ConstraintSolver
import qualified Type.Constraint.Gatherer as Gatherer
import Type.Constraint.Model (Constraint)
import qualified Type.Constraint.Printer as Printer
import qualified Type.Constraint.Module as Module
import qualified Type.ErrorPrinter as ErrorPrinter
import qualified Utils.Either as Either



run :: IO ()
run = do
    dir <- Directory.getCurrentDirectory
    putStrLn dir

    let filePath = "test/test"
    let parser = Module.moduleParser

    file <- readFile filePath
    -- putStrLn file

    putStrLn <| "\n\n--- PARSING ---\n"
    let parsedModule =
            Parser.runParser parser filePath file
                |> Either.mapLeft show
                |> Either.mapLeft (\s -> "PARSING ERROR: " ++ s)


    -- putStrLn <| Either.fold show TypePrinter.printModule parsedModule
    -- putStrLn <| show parsed


    -- putStrLn <| "\n\n--- TYPE CHECK ---\n"
    -- parsedModule
    --     |> map Inference.infer
    --     |> Either.fold show show
    --     |> \s -> putStrLn ("\n" ++ s)



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
                                (ErrorPrinter.printSolvingError file)
                            >> Either.mapLeft
                                (\s -> "CONSTRAINT SOLVING ERROR: \n" ++ s)
                        )
                    )
            ) :: [Either String Solution]


    putStrLn <| "\n\n--- TYPE INFERENCE ---\n"
    List.zip constraintResults solverResult
        |> traverse
            (\(gathering, solving) -> do
                printConstraintResults gathering
                putStrLn "\nSolution :"
                printSolutionResult solving
                putStrLn "\n---\n"
            )
        |> void


    putStrLn <| "\n\n--- CODE GENERATION ---\n"
    parsedModule
        |> map Generator.generate
        |> traverse putStrLn
        |> void


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
                |> map (Printer.printConstraint)
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
    let
        env =
            map
                (\t ->
                    case t of
                        M.Function { M.functionName, M.type_ } ->
                            (functionName, type_)
                )
                topLevels
    in do
    Module.gather topLevel
        |> Gatherer.withEnv env
        |> Gatherer.gatherConstraints
