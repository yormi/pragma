module Printer.Type.Constraint (print) where

import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty

import qualified AST.Identifier as Identifier
import qualified Printer.Type.Model as TypePrinter
import Printer.Utils (tab, indentAlign)
import qualified Type.Model as T
import Type.Constraint.Model (Constraint(..))
import qualified Type.Constraint.Model as Constraint
import Utils.String as String


alignTabNumber :: Int
alignTabNumber =
    6


print :: Constraint -> String
print constraint =
    case constraint of
        IfThenElse { condition, whenTrue, whenFalse, returnType } ->
            [ "If Then Else"
            , indentAlign
                alignTabNumber
                (tab ++ "Condition:")
                (printSimpleConstraintTypes
                    (Constraint.type_ condition)
                    T.Bool
                )
            , indentAlign
                alignTabNumber
                (tab ++ "Alternatives:")
                (printSimpleConstraint whenTrue whenFalse)
            , indentAlign
                alignTabNumber
                (tab ++ "Returns:")
                (printSimpleConstraintTypes
                    (T.Placeholder returnType)
                    (Constraint.type_ whenTrue)
                )
            ]
            |> String.mergeLines

        Application { functionReference, argTypes, returnType } ->
            let
                application =
                    List.foldl
                        (\type_ a ->
                            T.Function (T.FunctionType a type_)
                        )
                        (T.Placeholder returnType)
                        (NonEmpty.reverse argTypes)
            in
            [ "Application"
            , indentAlign alignTabNumber
                (tab ++ "Reference:")
                (TypePrinter.print functionReference)
            , indentAlign alignTabNumber
                (tab ++ "Actual:")
                (TypePrinter.print application)
            ]
            |> String.mergeLines

        Function { signatureType, params, body } ->
            let
                calculated =
                    List.foldl
                        (\type_ p ->
                            T.Function (T.FunctionType p type_)
                        )
                        body
                        (List.reverse params)
            in
            [ "Function Definition"
            , indentAlign alignTabNumber
                (tab ++ "Signature:")
                (TypePrinter.print signatureType)
            , indentAlign alignTabNumber
                (tab ++ "Actual:")
                (TypePrinter.print calculated)
            ]
            |> String.mergeLines

        Definition { dataId, actualType, returnType } ->
            [ "Generic"
            , indentAlign
                alignTabNumber
                (tab ++ "Name:")
                (Identifier.formatDataId dataId)
            , indentAlign
                alignTabNumber
                (tab ++ "Type:")
                (printSimpleConstraintTypes
                    (T.Placeholder returnType)
                    actualType
                )
            ]
            |> String.mergeLines


printSimpleConstraint :: Constraint.QuotedType -> Constraint.QuotedType -> String
printSimpleConstraint a b =
    printSimpleConstraintTypes
        (Constraint.type_ a)
        (Constraint.type_ b)


printSimpleConstraintTypes :: T.Type -> T.Type -> String
printSimpleConstraintTypes a b =
    TypePrinter.print a ++ "  vs.  " ++ TypePrinter.print b
