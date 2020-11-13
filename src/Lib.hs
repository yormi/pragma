module Lib (run) where

import GHC.IO.Encoding as Encoding
import qualified System.Directory as Directory
import System.IO (BufferMode(..), hSetBuffering, readFile, stdout)

import qualified Parser.Parser as Parser
import qualified Parser.Module as Module
import qualified Printer
import Type.Inference (infer)
import qualified Utils.Either as Either


run :: IO ()
run = do
    Encoding.setLocaleEncoding Encoding.utf8
    setFlushingStdoutRightAway

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
        |> \s -> putStrLn ("\n" ++ s)


setFlushingStdoutRightAway :: IO ()
setFlushingStdoutRightAway =
    hSetBuffering stdout LineBuffering
