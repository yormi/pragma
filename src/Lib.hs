module Lib (run) where

import qualified Data.List as List
import qualified Data.String as String
import qualified System.Directory as Directory
import System.IO (readFile)
import qualified Text.Layout.Table as Table

import qualified AST.Module as M
import Compiler (Compiler, CompilerError(..))
import qualified Compiler
import qualified Generator
import qualified Parser.Parser as Parser
import qualified Parser.Module as Module
import qualified Printer.AST.Module as ModulePrinter
import qualified Printer.CompilerError as CompilerErrorPrinter
import qualified Printer.Type.Constraint as ConstraintPrinter
import qualified Printer.Type.Solution as TypeSolutionPrinter
import qualified Printer.Type.SolverError as SolverErrorPrinter
import Type.Constraint.Solver.Model (Solution)
import qualified Type.Constraint.Solver.Solve as ConstraintSolver
import Type.Constraint.Model (Constraint)
import qualified Type.Constraint.Gatherer.Gather as Constraint
import qualified Utils.Either as Either
import qualified Utils.List as List
import qualified Utils.String as String


type GeneratedCode = String


run :: IO ()
run = do
    dir <- Directory.getCurrentDirectory
    putStrLn dir

    let filePath = "test/test"

    fileContent <- readFile filePath

    compilationResult <-
        compile filePath fileContent
            |> Compiler.run

    compilationResult
        |> Either.fold
            (map CompilerErrorPrinter.print
                >> traverse putStrLn
                >> void
            )
            (const <| return ())


compile :: String -> String -> Compiler GeneratedCode
compile filePath fileContent = do
    parsedModule <- parse filePath fileContent
    typeCheck parsedModule
    generateCode parsedModule


parse :: String -> String -> Compiler M.Module
parse filePath fileContent =
    let
        parser =
            Module.moduleParser
    in do
    printSectionHeader "PARSING"

    parsedModule <-
        Parser.runParser parser filePath fileContent
            |> Either.mapLeft ParsingError
            |> Compiler.fromEither

    tablePrint "Parsed"
        (ModulePrinter.print parsedModule)
        fileContent

    return parsedModule


typeCheck :: M.Module -> Compiler ()
typeCheck parsedModule@(M.Module topLevels) = do
    printSectionHeader "TYPE CHECK"
    constraintResults <- constraintGathering parsedModule
    solverResult <- solveConstraints constraintResults

    List.zip3 topLevels constraintResults solverResult
        |> map
            (\(topLevel, constraints, solution) ->
                (topLevel
                , [ constraints
                        |> map ConstraintPrinter.print
                        |> String.mergeLines
                    , ""
                    , "Solution :"
                    , TypeSolutionPrinter.print solution
                    , ""
                    , ""
                    , "---------------"
                    , ""
                    ]
                    |> String.mergeLines
                )
            )
        |> traverse
            (\(topLevel, solution) ->
                tablePrint
                    "Constraints"
                    solution
                    (ModulePrinter.printTopLevel topLevel)
            )
        |> void


constraintGathering :: M.Module -> Compiler [[Constraint]]
constraintGathering parsedModule@(M.Module topLevels) =
    map
        (Constraint.gather parsedModule
            >> Either.mapLeft ConstraintGatheringError
        )
        topLevels
        |> Compiler.fromEithers


solveConstraints :: [[Constraint]] -> Compiler [Solution]
solveConstraints constraintResults =
    constraintResults
        |> List.indexedMap
            (\index constraints ->
                ConstraintSolver.solve (index * 100) constraints
                    |> Either.mapLeft ConstraintSolvingError
            )
        |> Compiler.fromEithers


generateCode :: M.Module -> Compiler GeneratedCode
generateCode parsedModule = do
    printSectionHeader "GENERATING CODE"

    let generatedCode = Generator.generate parsedModule

    tablePrint "Generated NodeJS"
        generatedCode
        (ModulePrinter.print parsedModule)

    return generatedCode


--- PRINT ---


tablePrint :: String -> String -> String -> Compiler ()
tablePrint columnName leftColumn rightColumn =
    Table.tableString
        [ Table.fixedCol 80 Table.left
        , Table.column
            (Table.fixedUntil 80)
            Table.left
            Table.noAlign
            (Table.singleCutMark "...")
        ]
        Table.unicodeRoundS
        (Table.titlesH [columnName, "Pragma Source"])
        [ Table.colsAllG Table.center
            [ String.splitLines <| leftColumn
            , String.splitLines <| rightColumn
            ]
        ]
        |> print


printSectionHeader :: String -> Compiler ()
printSectionHeader sectionName =
    print <| "\n\n--- " ++ sectionName ++ " ---\n"


print :: String -> Compiler ()
print =
    putStrLn >> Compiler.liftIO
