module Printer.Type.Instanced (print) where

import qualified AST.Identifier as Identifier
import qualified Printer.Utils as Utils
import Type.Constraint.Solver.Instanced (InstanceId(..), InstancedType(..))
import qualified Type.Model as T
import qualified Utils.String as String


print :: InstancedType -> String
print type_ =
    case type_ of
        Bool ->
            "Bool"

        Int ->
            "Int"

        Float ->
            "Float"

        Char ->
            "Char"

        String ->
            "String"

        Function t1 t2 ->
            let
                formattedT1 =
                    t1
                        |> print
                        |> Utils.parenthesizeIfHasSpace
            in
            formattedT1 ++ " -> " ++ print t2

        Unbound name (T.TypePlaceholder p) ->
            Identifier.formatTypeVariableId name
                ++ show p

        Placeholder (T.TypePlaceholder p) ->
            "p" <> show p

        Custom typeId typeVariables ->
            Identifier.formatTypeId typeId
                ++ " "
                ++
                    (typeVariables
                        |> map print
                        |> String.mergeWords
                    )
