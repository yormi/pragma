module Printer.Type.Solution (print, printSolutionType) where

import qualified Data.Map as Map

import qualified Printer.Type.Instanced as InstancedTypePrinter
import qualified Printer.Type.Generic as GenericPrinter
import qualified Type.Model as Type
import Type.Constraint.Solver.SolutionModel (Solution)
import qualified Type.Constraint.Solver.SolutionModel as Solver
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


printSolutionType :: Solver.SolutionType -> String
printSolutionType solution =
    case solution of
        Solver.Instanced type_ ->
            InstancedTypePrinter.print type_

        Solver.Generic reference genericType ->
            GenericPrinter.print genericType
                ++ " ..... " ++ Reference.asString reference

        -- Solver.NamedType identifier genericVariables type_ ->
        --     let
        --         formattedGenericVariable :: String
        --         formattedGenericVariable =
        --             genericVariables
        --                 |> Set.toList
        --                 |> map Type.Variable
        --                 |> map TypePrinter.print
        --                 |> List.intercalate ", "
        --     in
        --     "∀ " ++ formattedGenericVariable ++ " | " ++ TypePrinter.print type_
        --         ++ " ..... " ++ Identifier.formatDataId identifier
