module Printer.Type.Constraint (print) where

import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty

import qualified AST.Identifier as Identifier
import qualified Printer.Type.Model as TypePrinter
import qualified Type.Model as T
import Type.Constraint.Model (Constraint(..))
import qualified Type.Constraint.Model as Constraint
import Utils.String as String


tabLength :: Int
tabLength =
    6


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


print :: Constraint -> String
print constraint =
    case constraint of
        IfThenElse { condition, whenTrue, whenFalse, returnType } ->
            [ "If Then Else"
            , indentAlign
                4
                (tab ++ "Condition:")
                (printSimpleConstraintTypes
                    (Constraint.type_ condition)
                    T.Bool
                )
            , indentAlign
                4
                (tab ++ "Alternatives:")
                (printSimpleConstraint whenTrue whenFalse)
            , indentAlign
                4
                (tab ++ "Returns:")
                (printSimpleConstraintTypes
                    (T.Variable returnType)
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
                        (T.Variable returnType)
                        (NonEmpty.reverse argTypes)
            in
            [ "Application"
            , indentAlign 4
                (tab ++ "Reference:")
                (TypePrinter.print functionReference)
            , indentAlign 4
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
            , indentAlign 4
                (tab ++ "Signature:")
                (TypePrinter.print signatureType)
            , indentAlign 4
                (tab ++ "Actual:")
                (TypePrinter.print calculated)
            ]
            |> String.mergeLines

        Generalized { identifier, actualType, returnType } ->
            [ "Generic"
            , indentAlign
                4
                (tab ++ "Name:")
                (Identifier.formatDataId identifier)
            , indentAlign
                4
                (tab ++ "Type:")
                (printSimpleConstraintTypes
                    (T.Variable returnType)
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
