module Printer.Type.Solution (print) where

import qualified Data.Map as Map

import qualified AST.Identifier as Identifier
import qualified Printer.Type.Model as TypePrinter
import qualified Type.Model as Type
import Type.Constraint.Solver.Model (Solution)
import qualified Type.Constraint.Solver.Model as Solver
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
                            ++ printSolutionType type_
                    )
                |> List.intercalate ("\n" ++ tab)
    in
    tab ++ solutions


printSolutionType :: Solver.SolutionType -> String
printSolutionType solution =
    case solution of
        Solver.InstanceType type_ ->
            TypePrinter.print type_

        Solver.NamedType identifier genericVariables type_ ->
            let
                formattedGenericVariable :: String
                formattedGenericVariable =
                    genericVariables
                        |> map Type.Variable
                        |> map TypePrinter.print
                        |> List.intercalate ", "
            in
            "∀ " ++ formattedGenericVariable ++ " | " ++ TypePrinter.print type_
                ++ " ..... " ++ Identifier.formatDataId identifier
