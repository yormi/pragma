module Printer.Type.Model (print, printAsParam, printKind) where

import qualified AST.Identifier as Identifier
import qualified Printer.Utils as Utils
import Type.Model (Kind(..), Type)
import qualified Type.Model as Type
import qualified Utils.String as String


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

        Type.Variable v ->
            Identifier.formatTypeVariableId v

        Type.Placeholder (Type.TypePlaceholder p) ->
            "p" <> show p

        Type.Custom typeId typeVariables ->
            Identifier.formatTypeId typeId
                ++ " "
                ++
                    (typeVariables
                        |> map print
                        |> String.mergeWords
                    )


printAsParam :: Type -> String
printAsParam type_ =
    case type_ of
        Type.Function _ ->
            "(" ++ print type_ ++ ")"

        _ ->
            print type_


printKind :: Kind -> String
printKind (Kind { typeVariables,  typeId }) =
    Identifier.formatTypeId typeId
        ++ " "
        ++
            (typeVariables
                |> map Identifier.formatTypeVariableId
                |> String.mergeWords
            )
