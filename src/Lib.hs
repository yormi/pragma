module Lib (run) where

import qualified System.Directory as Directory
import System.IO (readFile)

import qualified Parser.Parser as Parser
import qualified Parser.Module as Module
import qualified Printer
import Type.Inference (infer)
import qualified Utils.Either as Either


run :: IO ()
run = do
    dir <- Directory.getCurrentDirectory
    putStrLn dir

    let filePath = "test/test"
    let parser = Module.moduleParser

    file <- readFile filePath
    putStrLn file
    let parsed = Parser.runParser parser filePath file

    putStrLn <| "\n\n--- RESULT ---\n"
    putStrLn <| Either.fold show Printer.printModule parsed
    --putStrLn <| show parsed

    putStrLn <| "\n\n--- TYPE CHECK ---\n"
    parsed
        |> map infer
        |> Either.fold show show
        |> putStrLn
