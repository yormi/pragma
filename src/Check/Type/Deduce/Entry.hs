module Check.Type.Deduce.Entry
    ( Deductions
    , Error
    , deduceType
    ) where

import Control.Monad.Trans.Except (ExceptT)
import qualified Control.Monad.Trans.Except as Except
import qualified Control.Monad.State as State
import qualified GHC.Err as GHC

import AST.Identifier (TypeVariableId)
import qualified AST.Identifier as Identifier
import AST.TypeAnnotation (TypeAnnotation)
import qualified AST.TypeAnnotation as TA
import qualified Check.Type.Arrange as A
import Check.Type.Deduce.Deducer (Deducer, Deductions, Error(..))
import qualified Check.Type.Deduce.Deducer as Deducer
import qualified Check.Type.Deduce.Generalize as Generalize
import qualified Check.Type.Model.PrimitiveType as Primitive
import Check.Type.Model.Type (Type)
import qualified Check.Type.Model.Type as T
import qualified Utils.List as List
import qualified Utils.Map as Map
import qualified Utils.Set as Set


type VariableMapping =
    Map TypeVariableId T.InstancedType


deduceType :: [A.Expression] -> Either Error Deductions
deduceType arrangedExpressions =
    traverse deducer arrangedExpressions
        |> Deducer.run


deducer :: A.Expression -> Deducer ()
deducer arrangedExpression =
    case arrangedExpression of
        A.Primitive link quote primitiveType ->
            primitiveToType primitiveType
                |> Deducer.addDeduction link

        A.ContextReference link annotation -> do
            mapping <- variableMapping annotation
            type_ <- instantiateAnnotation mapping annotation
            Deducer.addDeduction link type_

        -- A.Future link  ->
        --     addDeduction link type_

        A.Definition { link, bodyLink } -> do
            deduction <- Deducer.lookupDeduction bodyLink
            case deduction of
                Just d ->
                    d
                        |> Generalize.generalize
                        |> Deducer.addDeduction link

                Nothing ->
                    ThisIsABug "The deduction must have been done before hand"
                        |> Deducer.fail
            -- TODO - GENERALIZE !?
            -- |> Generalize.generalize


primitiveToType :: Primitive.Type -> Type
primitiveToType primitive =
    case primitive of
        Primitive.Bool ->
            T.Bool

        Primitive.Int ->
            T.Int

        Primitive.Float ->
            T.Float

        Primitive.Char ->
            T.Char

        Primitive.String ->
            T.String


instantiateAnnotation :: VariableMapping -> TypeAnnotation -> Deducer Type
instantiateAnnotation mapping annotation = do
    case annotation of
        TA.Bool ->
            return <| T.Bool

        TA.Int ->
            return <| T.Int

        TA.Float ->
            return <| T.Float

        TA.Char ->
            return <| T.Char

        TA.String ->
            return <| T.String

        TA.Function { arg, returnType } -> do
            argType <- instantiateAnnotation mapping arg
            returningType <- instantiateAnnotation mapping returnType
            T.Function argType returningType
                |> return

        TA.Custom { typeName, args } -> do
            argTypes <- traverse (instantiateAnnotation mapping) args
            let name = Identifier.formatTypeId typeName
            T.Custom name argTypes
                |> return

        TA.Variable variableId ->
            case Map.lookup variableId mapping of
                Just placeholder ->
                    let
                        name =
                            Identifier.formatTypeVariableId variableId
                    in
                    T.Unbound name placeholder
                        |> return

                Nothing ->
                    GHC.error "Check.Type.Context - All the variable must have a mapping to a placeholder"


variableMapping :: TypeAnnotation -> Deducer VariableMapping
variableMapping annotation =
    let
        variables =
            TA.extractTypeVariables annotation
                |> Set.toList
    in do
    unboundTypes <- traverse (const Deducer.generateInstancedType) variables
    unboundTypes
        |> List.zip variables
        |> Map.fromList
        |> return
