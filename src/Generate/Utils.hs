module Generate.Utils
    ( formatConst
    , formatList
    , formatParamLine
    , formatReturn
    , formatString
    , indent
    , indentLines
    ) where

import qualified Utils.List as List


formatConst :: String -> [String] -> [String]
formatConst name lines =
    if List.length lines == 1 then
        [ "const " ++ name ++ " = " ++ join lines ]

    else
        [ "const " ++ name ++ " =" ]
            ++ indentLines lines


formatList :: [String] -> String
formatList elements =
    if List.isEmpty elements then
        "[]"

    else
        "[ "
            ++ List.intercalate ", " elements
            ++ " ]"


formatParamLine :: [String] -> String
formatParamLine params =
    case params of
        [] ->
            ""

        p : [] ->
            p ++ " => "

        _ ->
            List.intercalate " => " params ++ " => "


formatReturn :: [String] -> [String]
formatReturn lines =
    case lines of
        line : [] ->
            if List.contains ' ' line then
                [ "return (" ++ line ++ ")" ]

            else
                [ "return " ++ line ]

        _ ->
            [ "return (" ]
                ++ indentLines lines
                ++ [ ")" ]




formatString :: String -> String
formatString str =
    "\"" ++ str ++ "\""


indent :: String -> String
indent line =
    let
        indentation =
            "  "
    in
    indentation ++ line


indentLines :: [String] -> [String]
indentLines =
    map indent


