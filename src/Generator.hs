module Generator (generate) where

import qualified Data.List.NonEmpty as NonEmpty

import qualified AST.Expression as E
import qualified AST.Module as M
import qualified Printer
import qualified Utils.List as List
import qualified Utils.String as String


generate :: M.Module -> String
generate (M.Module topLevels) =
    map generateTopLevel topLevels
        |> List.intercalate "\n\n"


generateTopLevel :: M.TopLevel -> String
generateTopLevel topLevel =
    case topLevel of
        M.Function
            { type_
            , functionName
            , params
            , body
            }
            ->
            ["// " ++ Printer.printType type_ ]
                ++  generateConst functionName (generateFunction params body)
                |> String.mergeLines


generateExpression :: E.QuotedExpression -> [String]
generateExpression quotedExpression =
    case E.expression quotedExpression of
        E.Value value ->
            [generateValue value]

        E.Reference identifier ->
            [identifier]

        E.If { condition , whenTrue , whenFalse } ->
            [ "(() => {" ]
                ++
                    (([ "if (" ]
                        ++ generateExpression condition
                        ++ [ ") {" ]
                        |> join
                        |> List.singleton
                    )
                        ++
                            (generateExpression whenTrue
                                |> generateReturn
                                |> indentLines
                            )
                        ++ [ "} else {" ]
                        ++
                            ( generateExpression whenFalse
                                |> generateReturn
                                |> indentLines
                            )
                        ++ [ "}" ]
                        |> indentLines
                    )
                ++ [ "}) ()" ]

        E.LetIn { definitions , body } ->
            generateLet (NonEmpty.toList definitions) body

        E.CaseOf { element , cases } ->
            ["TODO :\t" ++ show (E.expression quotedExpression)]

        E.Lambda { params , body } ->
            generateFunction (NonEmpty.toList params) body

        E.Application { functionName , args } ->
            [functionName
                ++
                    (args
                        |> NonEmpty.toList
                        |> map
                            (\arg ->
                                "("
                                    ++ List.intercalate "; "
                                        (generateExpression arg)
                                    ++ ")"
                            )
                        |> join
                    )
            ]


generateLet :: [E.Definition] -> E.QuotedExpression -> [String]
generateLet definitions body =
    let
        generateLetDefinition def =
            case def of
                E.SimpleDefinition identifier definitionBody ->
                    generateConst identifier
                            (generateExpression definitionBody)
    in
    [ "(() => {" ]
        ++
            (definitions
                |> map generateLetDefinition
                |> join
                |> indentLines
            )
        ++
            ([ "return (" ]
                ++ indentLines (generateExpression body)
                ++ [")"]
                |> indentLines
            )
        ++ [ "}) ()" ]



generateFunction :: [E.Identifier] -> E.QuotedExpression -> [String]
generateFunction params body =
    if List.isEmpty params then
        generateExpression body
    else
        let
            paramLine =
                case params of
                    p : [] ->
                        p ++ " => "

                    _ ->
                        List.intercalate " => " params ++ " => "
        in
        case E.expression body of
            E.Value v ->
                paramLine
                    ++ generateValue v
                    |> List.singleton

            E.Reference identifier ->
                paramLine
                    ++ identifier
                    |> List.singleton

            _ ->
                [ paramLine ++ "{"]
                    ++
                        ([ "return (" ]
                            ++ indentLines (generateExpression body)
                            ++ [ ")" ]
                            |> indentLines
                        )
                    ++ [ "}" ]


generateReturn :: [String] -> [String]
generateReturn lines =
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


generateConst :: String -> [String] -> [String]
generateConst name lines =
    if List.length lines == 1 then
        [ "const " ++ name ++ " = " ++ join lines ]

    else
        [ "const " ++ name ++ " =" ]
            ++ indentLines lines


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


generateValue :: E.Value -> String
generateValue value =
    case value of
        E.Bool E.TrueLiteral ->
            "true"

        E.Bool E.FalseLiteral ->
            "false"

        E.Char c ->
            [c]

        E.Float n ->
            show n

        E.Int n ->
            show n

        E.String str ->
            str
