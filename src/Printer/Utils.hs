module Printer.Utils (indent, parenthesized, parenthesizeIfHasSpace) where

import qualified Utils.String as String


indent :: String -> String
indent =
    let
        fourSpaceIndent =
            "    "

        concatIndent line =
            if String.isEmpty line then
                line
            else
                fourSpaceIndent ++ line
    in
    String.splitLines
        >> (\lines ->
                case lines of
                    line : [] ->
                        concatIndent line

                    _ ->
                         lines
                            |> map concatIndent
                            |> String.mergeLines
           )


parenthesizeIfHasSpace :: String -> String
parenthesizeIfHasSpace str =
    if String.isContainingChar ' ' str then
        parenthesized str

    else
        str


parenthesized :: String -> String
parenthesized s =
    "(" ++ s ++ ")"
