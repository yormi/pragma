module Printer.Utils (indent, parenthesized) where

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


parenthesized :: String -> String
parenthesized s =
    "(" ++ s ++ ")"
