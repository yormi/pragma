module Printer.Type.Context
    ( print
    ) where

import qualified Data.Map as Map

import qualified AST.Identifier as Identifier
import qualified Printer.Type.Model as TypePrinter
import Printer.Utils (tab, indentAlign)
import Type.Constraint.Context.Model (Context)
import qualified Type.Constraint.Context.Model as Context
import qualified Type.Constraint.Context.Data as Data
import qualified Type.Constraint.Context.Constructor as Constructor
import qualified Type.Constraint.Context.Type as Type
import qualified Utils.String as String


alignTabNumber :: Int
alignTabNumber =
    6


print :: Context -> String
print context =
    ([ "Types"
    , "------"
    ]
        ++
            (context
                |> Context.type_
                |> Type.asMap
                |> Map.toList
                |> map
                    (\(typeId, kind) ->
                        indentAlign
                            alignTabNumber
                            (tab ++ Identifier.formatTypeId typeId ++ ": ")
                            (TypePrinter.printKind kind)
                    )
            )
        ++
            [ ""
            , ""
            , "Constructors"
            , "------"
            ]
        ++
            (context
                |> Context.constructor
                |> Constructor.asMap
                |> Map.toList
                |> map
                    (\(constructorId, type_) ->
                        indentAlign
                            alignTabNumber
                            (tab
                                ++ Identifier.formatConstructorId constructorId
                                ++ ": "
                            )
                            (TypePrinter.print type_)
                    )
            )
        ++
            [ ""
            , ""
            , "Data"
            , "------"
            ]
        ++
            (context
                |> Context.data_
                |> Data.asMap
                |> Map.toList
                |> map
                    (\(dataId, type_) ->
                        indentAlign
                            alignTabNumber
                            (tab
                                ++ Identifier.formatDataId dataId
                                ++ ": "
                            )
                            (TypePrinter.print type_)
                    )
            )
        |> String.mergeLines
    )
