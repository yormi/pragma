module Printer.Type.Solution (print) where

import qualified Data.Map as Map

import qualified Printer as TypePrinter
import Type.Constraint.Solver.Model (Solution)
import qualified Utils.List as List
import qualified Utils.Tuple as Tuple


tab :: String
tab =
    "      "


print :: Solution -> String
print solution =
    let
        solutions =
            solution
                |> Map.toList
                |> map (Tuple.mapFirst (\n -> "a" ++ show n))
                |> map
                    (\(variable, type_) ->
                        variable
                            ++ " :: "
                            ++ TypePrinter.printTypeSolution type_
                    )
                |> List.intercalate ("\n" ++ tab)
    in
    tab ++ solutions
