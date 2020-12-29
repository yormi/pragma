module CompilationStep
    ( GeneratedCode
    , PrintPreferences(..)
    , generateCode
    , parse
    , print
    , typeCheck
    ) where

import qualified Control.Monad as Monad
import qualified Data.List as List
import qualified Text.Layout.Table as Table

import qualified AST.Module as M
import Compiler (Compiler, CompilerError(..))
import qualified Compiler
import qualified Generator
import qualified Parser.Parser as Parser
import qualified Parser.Module as Module
import qualified Printer.AST.Module as ModulePrinter
import qualified Printer.Type.Constraint as ConstraintPrinter
import qualified Printer.Type.Context as ContextPrinter
import qualified Printer.Type.Solution as TypeSolutionPrinter
import qualified Type.Constraint.Context.Model as Context
import Type.Constraint.Context.Model (Context)
import Type.Constraint.Model (Constraint)
import qualified Type.Constraint.Gatherer.Model as Gatherer
import qualified Type.Constraint.Gatherer.Module as Module
import Type.Constraint.Solver.Model (Solution)
import qualified Type.Constraint.Solver.Solve as ConstraintSolver
import Type.Model as T
import qualified Utils.Either as Either
import qualified Utils.String as String


type GeneratedCode = String


data PrintPreferences
    = PrintPreferences
        { parseResult :: Bool
        , contextResult :: Bool
        , gatherResult :: Bool
        , solveResult :: Bool
        , generateResult :: Bool
        }


parse :: PrintPreferences -> String -> String -> Compiler M.Module
parse printPreferences filePath fileContent =
    let
        parser =
            Module.moduleParser
    in do
    printSectionHeader "PARSING"

    parsedModule <-
        Parser.runParser parser filePath fileContent
            |> Either.mapLeft ParsingError
            |> Compiler.fromEither

    tablePrint
       (parseResult printPreferences)
       "Parsed"
        (ModulePrinter.print parsedModule)
        fileContent

    return parsedModule


typeCheck :: PrintPreferences -> M.Module -> Compiler ()
typeCheck printPreferences parsedModule@(M.Module topLevels) = do
    printSectionHeader "ANALYZE CONTEXT"
    context <-
        Context.context topLevels
            |> Either.mapLeft ContextError
            |> Compiler.fromEither

    tablePrint
        (contextResult printPreferences)
        "Context"
        (ContextPrinter.print context)
        (ModulePrinter.print parsedModule)

    printSectionHeader "TYPE CHECK"
    constraintResults <- constraintGathering context parsedModule
    solverResult <- solveConstraints constraintResults

    List.zip3 topLevels constraintResults solverResult
        |> map
            (\(topLevel, constraints, solution) ->
                (topLevel
                , formatTypeCheckResult constraints solution
                )
            )
        |> traverse
            (\(topLevel, solution) ->
                tablePrint
                    (gatherResult printPreferences)
                    "Constraints"
                    solution
                    (ModulePrinter.printTopLevel topLevel)
            )
        |> void


formatTypeCheckResult :: [Constraint] -> ConstraintSolver.Solution -> String
formatTypeCheckResult constraints solution =
    (constraints
        |> map ConstraintPrinter.print
        |> List.intersperse ""
        |> String.mergeLines
    )
        ++
            ([ ""
            , ""
            , "-----------------------------------------------"
            , ""
            , "Solution :"
            , TypeSolutionPrinter.print solution
            , ""
            , ""
            ]
                |> String.mergeLines
            )


constraintGathering :: Context -> M.Module -> Compiler [[Constraint]]
constraintGathering context (M.Module topLevels) = do
    map
        (Module.gather
            >> Gatherer.gatherConstraints context
            >> Either.mapLeft ConstraintGatheringError
        )
        topLevels
        |> Compiler.fromEithers


solveConstraints :: [[Constraint]] -> Compiler [Solution]
solveConstraints constraintResults =
    constraintResults
        |> map
            ( ConstraintSolver.solve (T.TypePlaceholder 100)
                >> Either.mapLeft ConstraintSolvingError
            )
        |> Compiler.fromEithers


generateCode :: PrintPreferences -> M.Module -> Compiler GeneratedCode
generateCode printPreferences parsedModule = do
    printSectionHeader "GENERATING CODE"

    let generatedCode = Generator.generate parsedModule

    tablePrint
        (generateResult printPreferences)
        "Generated NodeJS"
        generatedCode
        (ModulePrinter.print parsedModule)

    return generatedCode


--- PRINT ---


tablePrint :: Bool -> String -> String -> String -> Compiler ()
tablePrint shouldPrint columnName leftColumn rightColumn =
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
        |> Monad.when shouldPrint


printSectionHeader :: String -> Compiler ()
printSectionHeader sectionName =
    print <| "\n\n--- " ++ sectionName ++ " ---\n"


print :: String -> Compiler ()
print =
    putStrLn >> Compiler.liftIO
