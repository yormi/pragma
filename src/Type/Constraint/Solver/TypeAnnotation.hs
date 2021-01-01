module Type.Constraint.Solver.TypeAnnotation
    (fromType
    , isTypeMatching
    ) where

import AST.TypeAnnotation (TypeAnnotation)
import qualified AST.TypeAnnotation as TA
import qualified Type.Model as T
import Type.Constraint.Solver.Model (Solver)
import qualified Type.Constraint.Solver.Model as Solver
import qualified Type.Constraint.Solver.Solution as Solution
import qualified Utils.List as List


isTypeMatching :: TypeAnnotation -> T.Type -> Bool
isTypeMatching annotation type_ =
    case (annotation, type_) of
        (TA.Bool, T.Bool) ->
            True

        (TA.Int, T.Int) ->
            True

        (TA.Float, T.Float) ->
            True

        (TA.Char, T.Char) ->
            True

        (TA.String, T.String) ->
            True

        (TA.Function argA returnTypeA
            , T.Function (T.FunctionType argB returnTypeB)
            ) -> do
            isTypeMatching argA argB
                && isTypeMatching returnTypeA returnTypeB

        (TA.Custom typeName argsA, T.Custom typeId argsB) ->
            typeName == typeId
                && List.length argsA == List.length argsB
                &&
                    (List.zip argsA argsB
                        |> List.all (\(a, b) -> isTypeMatching a b)
                    )

        (TA.Variable _, T.Placeholder _) ->
            True

        _ ->
            False


fromType :: T.Type -> Solver TypeAnnotation
fromType type_ = do
    precised <- Solution.mostPrecised type_
    case precised of
        T.Bool ->
            return TA.Bool

        T.Int ->
            return TA.Int

        T.Float ->
            return TA.Float

        T.Char ->
            return TA.Char

        T.String ->
            return TA.String

        T.Function (T.FunctionType arg returnType) -> do
            argAnnotation <- fromType arg
            returnAnnotation <- fromType returnType
            TA.Function argAnnotation returnAnnotation
                |> return

        T.Custom typeName args -> do
            argsAnnotation <- traverse fromType args
            TA.Custom typeName argsAnnotation
                |> return

        T.Placeholder _ ->
            Solver.nextVariable
                |> map TA.Variable
