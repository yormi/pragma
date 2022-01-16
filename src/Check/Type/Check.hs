module Check.Type.Check
    ( check
    ) where

import qualified Check.Type.Arrange as A
import qualified Check.Type.Check.InstantiateAnnotation as InstantiateAnnotation
import Check.Type.Check.Model.Checker (Checker)
import qualified Check.Type.Check.Model.Checker as Checker
import qualified Check.Type.Check.Model.InstancedType as I
import Check.Type.Constraint (Constraint)
import qualified Check.Type.Constraint as C
import qualified Check.Type.Model.PrimitiveType as Primitive
import qualified Utils.Either as Either


check :: [ Constraint ] -> Maybe Checker.Error
check constraints =
    traverse checkConstraint constraints
        |> Checker.run
        |> Either.toMaybeError


checkConstraint :: Constraint -> Checker ()
checkConstraint constraint =
    case constraint of
        C.Primitive { link , quote , primitiveType } ->
            checkPrimitive link primitiveType

        C.ContextReference { link , annotation } -> do
            instantiated <- InstantiateAnnotation.instantiate annotation
            Checker.updateSolution link (Checker.Instanced instantiated)

        C.Future { link } ->
            Checker.linkSolution link
                |> void

        -- Definition { link , bodyLink } ->

        C.IfCondition link -> do
            solution <- Checker.linkSolution link
            if solution == Checker.Instanced I.Bool then
                return ()

            else
                Checker.fail <| Checker.IfConditionMustBeABool solution


        -- IfAlternatives { whenTrue , whenFalse } ->

        -- Redirect { from , to } ->
        --     updateSolution 


checkPrimitive :: A.Link -> Primitive.Type -> Checker ()
checkPrimitive link primitiveType =
    let
        type_ =
            case primitiveType of
                Primitive.Bool ->
                    I.Bool

                Primitive.Int ->
                    I.Int

                Primitive.Float ->
                    I.Float

                Primitive.Char ->
                    I.Char

                Primitive.String ->
                    I.String
    in
    Checker.updateSolution link (Checker.Instanced type_)
