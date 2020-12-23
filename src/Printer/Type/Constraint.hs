module Printer.Type.Constraint (print) where

import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.String as String

import qualified Printer as TypePrinter
import qualified Type as T
import Type.Constraint.Model (Constraint(..))
import qualified Type.Constraint.Model as Constraint
import Utils.String as String


tab :: String
tab =
    "      "


print :: Constraint -> String
print constraint =
    case constraint of
        IfThenElse { condition, whenTrue, whenFalse, returnType } ->
            [ "If Then Else"
            , tab ++ "Condition:" ++ tab
                ++ printSimpleConstraintTypes
                    (Constraint.type_ condition)
                    T.Bool
            , tab ++ "Alternatives:" ++ tab
                ++ printSimpleConstraint whenTrue whenFalse
            , tab ++ "Returns:" ++ tab
                ++ printSimpleConstraintTypes
                    (T.Variable returnType)
                    (Constraint.type_ whenTrue)
            ]
            |> String.unlines

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
            , tab ++ printSimpleConstraintTypes application functionReference
            ]
            |> String.unlines

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
            , tab ++ printSimpleConstraintTypes calculated signatureType
            ]
            |> String.unlines

        Generalized { actualType, returnType } ->
            [ "Generic"
            , tab
                ++ printSimpleConstraintTypes
                    (T.Variable returnType)
                    actualType
            ]
            |> String.mergeLines


printSimpleConstraint :: Constraint.QuotedType -> Constraint.QuotedType -> String
printSimpleConstraint a b =
    printSimpleConstraintTypes
        (Constraint.type_ a)
        (Constraint.type_ b)


printSimpleConstraintTypes :: T.Type -> T.Type -> String
printSimpleConstraintTypes a b =
    TypePrinter.printType a ++ "  vs.  " ++ TypePrinter.printType b
