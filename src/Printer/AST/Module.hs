module Printer.AST.Module (print, printTopLevel) where

import qualified Data.List.NonEmpty as NonEmpty

import AST.Module (DataChoice(..), Module(..), TopLevel(..))
import qualified Printer.AST.Expression as ExpressionPrinter
import qualified AST.Identifier as Identifier
import qualified Printer.AST.TypeAnnotation as TypeAnnotation
import qualified Printer.Utils as Utils
import qualified Utils.List as List
import qualified Utils.String as String


print :: Module -> String
print (Module functions) =
    functions
        |> map printTopLevel
        |> List.intersperse "\n"
        |> String.mergeLines


printTopLevel :: TopLevel -> String
printTopLevel element =
    case element of
        Function
            { typeAnnotation
            , functionName
            , params
            , body
            }
            ->
            let
                typeLine =
                    typeAnnotation
                        |> TypeAnnotation.print
                        |>
                            (\t ->
                                Identifier.formatDataId functionName
                                    ++ " : " ++ t ++ "\n"
                            )


            in
            typeLine
                ++ Identifier.formatDataId functionName ++ " "
                ++ (map Identifier.formatDataId params |> String.mergeWords)
                ++ " =\n"
                ++ (Utils.indent <| ExpressionPrinter.print body)

        SumType { typeName, typeVariables, dataChoices } ->
            ["type "
                ++ Identifier.formatTypeId typeName
                ++ " "
                ++
                    (typeVariables
                        |> map Identifier.formatTypeVariableId
                        |> String.mergeWords
                    )
            , Utils.indent <| "= " ++ printDataChoice (NonEmpty.head dataChoices)
            ]
            ++
                (NonEmpty.tail dataChoices
                    |> map printDataChoice
                    |> map (\s -> "| " ++ s)
                    |> map Utils.indent
                )
            |> String.mergeLines


printDataChoice :: DataChoice -> String
printDataChoice (DataChoice _ tag args) =
    if List.isEmpty args then
        Identifier.formatConstructorId tag
    else
        Identifier.formatConstructorId tag
            ++ " "
            ++ (args
                    |> map TypeAnnotation.print
                    |> String.mergeWords
                )


