module Printer.CompilerError (print) where

import Compiler (CompilerError(..))


print :: CompilerError -> String
print =
    show
