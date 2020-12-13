module Type.Constraint.Printer (printConstraint) where

import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.String as String

import qualified Printer as TypePrinter
import qualified Type as T
import Type.Constraint.Model (Constraint(..))
import qualified Type.Constraint.Model as Constraint


printConstraint :: Constraint -> String
printConstraint constraint =
    case constraint of
        Simple { newComing, concludedFrom } ->
            [ "Simple Constraint"
            , "\t" ++ printSimpleConstraint newComing concludedFrom
            ]
            |> String.unlines

        IfThenElse { condition, whenTrue, whenFalse, returnType } ->
            [ "If Then Else"
            , "\tCondition:\t"
                ++ printSimpleConstraintTypes
                    (Constraint.type_ condition)
                    T.Bool
            , "\tAlternatives:\t"
                ++ printSimpleConstraint whenTrue whenFalse
            , "\tReturns:\t"
                ++ printSimpleConstraintTypes
                    returnType
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
                        returnType
                        (NonEmpty.reverse argTypes)
            in
            [ "Application"
            , "\t" ++ printSimpleConstraintTypes application functionReference
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
            , "\t" ++ printSimpleConstraintTypes calculated signatureType
            ]
            |> String.unlines


printSimpleConstraint :: Constraint.Element -> Constraint.Element -> String
printSimpleConstraint a b =
    printSimpleConstraintTypes
        (Constraint.type_ a)
        (Constraint.type_ b)


printSimpleConstraintTypes :: T.Type -> T.Type -> String
printSimpleConstraintTypes a b =
    TypePrinter.printType a ++ "  vs.  " ++ TypePrinter.printType b
