module Type.Constraint.Printer (printConstraint) where

import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.String as String

import qualified Printer as TypePrinter
import qualified Type as T
import Type.Constraint.Model (Constraint(..))


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
            , "\tCondition:\t" ++ printSimpleConstraint condition T.Bool
            , "\tAlternatives:\t" ++ printSimpleConstraint whenTrue whenFalse
            , "\tReturns:\t" ++ printSimpleConstraint returnType whenTrue
            ]
            |> String.unlines

        Application { functionReference, args, returnType } ->
            let
                application =
                    List.foldl
                        (\type_ a ->
                            T.Function (T.FunctionType a type_)
                        )
                        returnType
                        (NonEmpty.reverse args)
            in
            [ "Application"
            , "\t" ++ printSimpleConstraint application functionReference
            ]
            |> String.unlines

        Function { functionType, params, body } ->
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
            , "\t" ++ printSimpleConstraint calculated functionType
            ]
            |> String.unlines


printSimpleConstraint :: T.Type -> T.Type -> String
printSimpleConstraint a b =
    TypePrinter.printType a ++ "  vs.  " ++ TypePrinter.printType b
