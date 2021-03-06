module Printer.Utils
    ( indent
    , parenthesized
    , parenthesizeIfFunction
    , parenthesizeIfHasSpace
    , tab
    , tabLength
    , indentAlign
    ) where

import qualified Utils.List as List
import qualified Utils.String as String


tabLength :: Int
tabLength =
    4


tab :: String
tab =
    List.replicate tabLength ' '


indentAlign :: Int -> String -> String -> String
indentAlign numberOfIndents beginning end =
    let
        column =
            numberOfIndents * tabLength
    in
    String.padRight column beginning
        ++ end


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


parenthesizeIfFunction :: String -> String
parenthesizeIfFunction str =
    if String.containsString "->" str then
        parenthesized str

    else
        str


parenthesizeIfHasSpace :: String -> String
parenthesizeIfHasSpace str =
    if String.containsChar ' ' str then
        parenthesized str

    else
        str


parenthesized :: String -> String
parenthesized s =
    "(" ++ s ++ ")"
