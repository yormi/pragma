module Generator (generate) where

import qualified Data.List.NonEmpty as NonEmpty

import qualified AST.Expression as E
import AST.Identifier (DataId)
import qualified AST.Identifier as Identifier
import qualified AST.Module as M
import qualified Printer.AST.TypeAnnotation as TypeAnnotationPrinter
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
            { typeAnnotation
            , functionName
            , params
            , body
            }
            ->
            [ "//  " ++ TypeAnnotationPrinter.print typeAnnotation ]
                ++  generateConst
                    (Identifier.formatDataId functionName)
                    (generateFunction params body)
                |> String.mergeLines

        M.SumType { typeName, dataChoices } ->
            [ "//  Not sur how to encode this yet" ]
                ++  generateConst (Identifier.formatTypeId typeName)
                    [ "// "
                        ++
                            ( dataChoices
                                |> NonEmpty.toList
                                |> map M.tag
                                |> map Identifier.formatConstructorId
                                |> String.mergeWords
                            )
                    ]
                |> String.mergeLines


generateExpression :: E.QuotedExpression -> [String]
generateExpression quotedExpression =
    case E.expression quotedExpression of
        E.Value value ->
            [generateValue value]

        E.Reference identifier ->
            [Identifier.formatReferenceId identifier]

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
            [Identifier.formatReferenceId functionName
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
                    generateConst
                        (Identifier.formatDataId identifier)
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



generateFunction :: [DataId] -> E.QuotedExpression -> [String]
generateFunction params body =
    if List.isEmpty params then
        generateExpression body
    else
        let
            formattedParams =
                map Identifier.formatDataId params

            paramLine =
                case formattedParams of
                    p : [] ->
                        p ++ " => "

                    _ ->
                        List.intercalate " => " formattedParams ++ " => "
        in
        case E.expression body of
            E.Value v ->
                paramLine
                    ++ generateValue v
                    |> List.singleton

            E.Reference identifier ->
                paramLine
                    ++ (Identifier.formatReferenceId identifier)
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
