module Printer.Type.Solution (print, printSolutionType) where

import qualified Data.Map as Map

import qualified Printer.Type.Instanced as InstancedTypePrinter
import qualified Printer.Type.Generic as GenericPrinter
import qualified Printer.Utils as Utils
import qualified Type.Model as Type
import Type.Constraint.Solver.Model.Solution (Solution)
import qualified Type.Constraint.Solver.Model.Solution as Solution
import qualified Type.Constraint.Reference as Reference
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
                |> map
                    (Tuple.mapFirst
                        (\(Type.TypePlaceholder n) -> "p" ++ show n)
                    )
                |> map
                    (\(variable, type_) ->
                        variable
                            ++ " :: "
                            ++ printSolutionType type_
                    )
                |> List.intercalate ("\n" ++ tab)
    in
    tab ++ solutions


printSolutionType :: Solution.SolutionType -> String
printSolutionType solution =
    case solution of
        Solution.Instanced type_ ->
            InstancedTypePrinter.print type_

        Solution.Generic reference genericType ->
            Utils.indentAlign
                9
                (GenericPrinter.print genericType)
                (Reference.asString reference)
