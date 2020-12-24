module Printer.CompilerError (print) where

import Compiler (CompilerError(..))
import qualified Printer.Type.SolverError as SolverErrorPrinter

print :: String -> CompilerError -> String
print sourceCode error =
    case error of
        ParsingError e ->
            show e

        ConstraintGatheringError e ->
            show e

        ConstraintSolvingError e ->
            SolverErrorPrinter.print sourceCode e
