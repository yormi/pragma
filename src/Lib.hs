module Lib (run) where

import qualified System.Directory as Directory
import System.IO (readFile)

import Compiler (Compiler)
import qualified Compiler as Compiler
import qualified CompilationStep
import qualified Printer.CompilerError as CompilerErrorPrinter
import qualified Utils.Either as Either



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
            (map (CompilerErrorPrinter.print fileContent)
                >> traverse putStrLn
                >> void
            )
            (const <| return ())


compile :: String -> String -> Compiler CompilationStep.GeneratedCode
compile filePath fileContent = do
    parsedModule <- CompilationStep.parse filePath fileContent
    CompilationStep.typeCheck parsedModule
    CompilationStep.generateCode parsedModule
