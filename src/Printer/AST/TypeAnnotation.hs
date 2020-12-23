module Printer.AST.TypeAnnotation (print) where

import AST.TypeAnnotation (TypeAnnotation(..))
import qualified Printer.Utils as Utils


print :: TypeAnnotation -> String
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

        Variable identifier ->
            identifier
