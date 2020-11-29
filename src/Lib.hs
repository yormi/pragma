module Lib (run) where

import qualified Data.Bifunctor as Bifunctor
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.String as String
import qualified System.Directory as Directory
import System.IO (readFile)

import qualified AST.Module as M
import qualified Parser.Parser as Parser
import qualified Parser.Module as Module
import qualified Printer as TypePrinter
import Type.ConstraintSolver (TypeSolution)
import qualified Type.ConstraintSolver as ConstraintSolver
import Type.Constraint.Gatherer (ConstraintError)
import Type.Constraint.Model (Constraint)
import qualified Type.Constraint.Printer as Printer
import qualified Type.ErrorPrinter as ErrorPrinter
import qualified Type.Inference as Inference
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
                        (ConstraintSolver.solve
                            >> Either.mapLeft ErrorPrinter.printSolvingError
                            >> Either.mapLeft
                                (\s -> "CONSTRAINT SOLVING ERROR: \n" ++ s)
                        )
                    )
            ) :: [Either String TypeSolution]


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


constraintGathering :: M.Module -> [Either String [Constraint]]
constraintGathering parsedModule@(M.Module topLevels) =
    map
        (gatherFunctionConstraints parsedModule
            >> Either.mapLeft show
            >> Either.mapLeft (\s -> "CONSTRAINT GATHERING ERROR: " ++ s)
        )
        topLevels


gatherFunctionConstraints
    :: M.Module
    -> M.TopLevel
    -> Either ConstraintError [Constraint]
gatherFunctionConstraints parsedModule =
    Inference.gatherConstraints parsedModule


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


printSolutionResult :: Either String TypeSolution -> IO ()
printSolutionResult result =
    case result of
        Right solution ->
            printSolution solution

        Left e ->
            putStrLn e


printSolution :: TypeSolution -> IO ()
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
                            ++ TypePrinter.printType type_
                    )
                |> List.intercalate "\n\t"
    in
    putStrLn <| "\t" ++ solutions
