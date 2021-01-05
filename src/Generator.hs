module Generator (generate) where

import qualified Data.List.NonEmpty as NonEmpty

import qualified AST.Expression as E
import AST.Identifier (DataId)
import qualified AST.Identifier as Identifier
import qualified AST.Module as M
import qualified Generate.Utils as Utils
import qualified Printer.AST.TypeAnnotation as TypeAnnotationPrinter
import qualified Utils.List as List
import qualified Utils.Maybe as Maybe
import qualified Utils.String as String


generate :: M.Module -> String
generate (M.Module topLevels) =
    map generateTopLevel topLevels
        |> List.intercalate "\n\n"


generateTopLevel :: M.TopLevel -> String
generateTopLevel topLevel =
    (case topLevel of
        M.Function
            { typeAnnotation
            , functionName
            , params
            , body
            }
            ->
            [ "//  " ++ TypeAnnotationPrinter.print typeAnnotation ]
                ++  Utils.formatConst
                    (Identifier.formatDataId functionName)
                    (generateFunction params body)

        M.SumType { typeName, dataChoices } ->
            [ "//  " ++ Identifier.formatTypeId typeName ]
                ++
                    ( dataChoices
                        |> NonEmpty.toList
                        |> bind generateConstructor
                    )

--        M.Record {} ->
--            [ "Record - TODO" ]
    )
                |> String.mergeLines


generateConstructor :: M.DataChoice -> [String]
generateConstructor (M.DataChoice { tag, args }) =
    let
        argNames =
            List.indexedMap
                (\index _ -> "_" ++ show index)
                args

        paramLine =
            if List.isEmpty args then
                Nothing

            else
                Just <| Utils.formatParamLine argNames

        object =
            [ "({"
            , Utils.indent
                "tag: "
                    ++ Utils.formatString
                        (Identifier.formatConstructorId tag)
                    ++ ","
            ]
                ++ contents
                ++ [ "})" ]

        contents =
            if List.isEmpty argNames then
                []
            else
                [ Utils.indent <|
                    "contents: " ++ Utils.formatList argNames
                ]
    in
    Utils.formatConst
        (Identifier.formatConstructorId tag)
        (Maybe.toList paramLine ++ Utils.indentLines object)


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
                                |> Utils.formatReturn
                                |> Utils.indentLines
                            )
                        ++ [ "} else {" ]
                        ++
                            ( generateExpression whenFalse
                                |> Utils.formatReturn
                                |> Utils.indentLines
                            )
                        ++ [ "}" ]
                        |> Utils.indentLines
                    )
                ++ [ "}) ()" ]

        E.LetIn { definitions , body } ->
            generateLet (NonEmpty.toList definitions) body

        E.CaseOf {} ->
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
                    Utils.formatConst
                        (Identifier.formatDataId identifier)
                        (generateExpression definitionBody)
    in
    [ "(() => {" ]
        ++
            (definitions
                |> map generateLetDefinition
                |> join
                |> Utils.indentLines
            )
        ++
            ([ "return (" ]
                ++ Utils.indentLines (generateExpression body)
                ++ [")"]
                |> Utils.indentLines
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
                Utils.formatParamLine formattedParams
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
                            ++ Utils.indentLines (generateExpression body)
                            ++ [ ")" ]
                            |> Utils.indentLines
                        )
                    ++ [ "}" ]


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
            Utils.formatString str
