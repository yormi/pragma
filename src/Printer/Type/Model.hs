module Printer.Type.Model (print, printAsParam) where

import qualified AST.Identifier as Identifier
import qualified Printer.Utils as Utils
import Type.Model (Type)
import qualified Type.Model as Type


print :: Type -> String
print type_ =
    case type_ of
        Type.Bool ->
            "Bool"

        Type.Int ->
            "Int"

        Type.Float ->
            "Float"

        Type.Char ->
            "Char"

        Type.String ->
            "String"

        Type.Function (Type.FunctionType t1 t2) ->
            let
                formattedT1 =
                    case t1 of
                        Type.Function _ ->
                            t1
                                |> print
                                |> Utils.parenthesized
                        _ ->
                            print t1
            in
            formattedT1 ++ " -> " ++ print t2

        Type.Variable n ->
            "v" <> show n

        Type.Placeholder n ->
            "p" <> show n

        Type.Custom typeId ->
            Identifier.formatTypeId typeId


printAsParam :: Type -> String
printAsParam type_ =
    case type_ of
        Type.Function _ ->
            "(" ++ print type_ ++ ")"

        _ ->
            print type_