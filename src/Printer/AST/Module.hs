module Printer.AST.Module (print, printTopLevel) where

import qualified Data.List.NonEmpty as NonEmpty

import AST.Module (DataChoice(..), Module(..), TopLevel(..))
import qualified Printer.AST.Expression as ExpressionPrinter
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
                        |> (\t -> functionName ++ " : " ++ t ++ "\n")


            in
            typeLine
                ++ functionName ++ " "
                ++ String.mergeWords params
                ++ " =\n"
                ++ (Utils.indent <| ExpressionPrinter.print body)

        SumType { typeName, dataChoices } ->
            ["type " ++ typeName
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
        tag
    else
        tag ++ " "
            ++ (args
                    |> map TypeAnnotation.print
                    |> String.mergeWords
                )


