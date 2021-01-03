module Printer.Type.Generic (print) where

import qualified AST.Identifier as Identifier
import Type.Constraint.Solver.Generic (GenericType(..))
import Type.Constraint.Solver.Instanced (InstanceId(..))
import qualified Type.Model as T
import qualified Printer.Utils as Utils
import qualified Utils.String as String


print :: GenericType -> String
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
                    case t1 of
                        Function _ _ ->
                            t1
                                |> print
                                |> Utils.parenthesized
                        _ ->
                            print t1
            in
            formattedT1 ++ " -> " ++ print t2

        Custom { typeName, args } ->
            Identifier.formatTypeId typeName
                ++ " "
                ++
                    (args
                        |> map print
                        |> String.mergeWords
                    )

        Instance (InstanceId n) ->
            "i" ++ show n

        Placeholder (T.TypePlaceholder n) ->
            "p" ++ show n

        Variable identifier ->
            Identifier.formatTypeVariableId identifier
