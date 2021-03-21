module Check.Type.Deduce
    ( Deductions
    , Error
    , deduceType
    ) where

import Control.Monad.State (StateT)
import qualified Control.Monad.State as State
import qualified GHC.Err as GHC

import AST.Identifier (TypeVariableId)
import qualified AST.Identifier as Identifier
import AST.TypeAnnotation (TypeAnnotation)
import qualified AST.TypeAnnotation as TA
import qualified Check.Type.Arrange as A
import Check.Type.Model (Type)
import qualified Check.Type.Model.PrimitiveType as Primitive
import qualified Check.Type.Model as T
import qualified Utils.List as List
import qualified Utils.Map as Map
import qualified Utils.Set as Set


type Deducer a =
    StateT State (Either Error) a


data State =
    State
        { deductions :: Deductions
        , nextInstancedType :: T.InstancedType
        }


type Deductions =
    Map A.Link Type


newtype Error
    = ThisIsABug String
    deriving (Eq, Show)


type VariableMapping =
    Map TypeVariableId T.InstancedType


addDeduction :: A.Link -> Type -> Deducer ()
addDeduction link type_ =
    State.modify
        (\state ->
            state
                |> deductions
                |> Map.insert link type_
                |> \ds -> state { deductions = ds }
        )


generateInstancedType :: Deducer T.InstancedType
generateInstancedType = do
    instancedType@(T.InstancedType n) <- State.gets nextInstancedType
    let next = T.InstancedType <| n + 1
    State.modify (\state -> state { nextInstancedType = next })
    return instancedType


-- ACTION


deduceType :: [A.Expression] -> Either Error Deductions
deduceType arrangedExpressions =
    let
        initialState =
            State
                Map.empty
                (T.InstancedType 0)
    in
    traverse deducer arrangedExpressions
        |> flip State.execStateT initialState
        |> map deductions


deducer :: A.Expression -> Deducer ()
deducer arrangedExpression =
    case arrangedExpression of
        A.Primitive link quote primitiveType ->
            let
                type_ =
                    case primitiveType of
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
            in
            addDeduction link type_

        A.ContextReference link annotation -> do
            mapping <- variableMapping annotation
            type_ <- annotationToType mapping annotation
            addDeduction link type_

        -- A.Future link  ->
        --     addDeduction link type_

        -- Definition
        --   { link :: Link
        --   , futurePlaceholder :: F.Placeholder
        --   , body :: Link
        --   }
            -- TODO - GENERALIZE !?

        -- OrderedIf
        --   { condition :: Link
        --   , whenTrue :: Link
        --   , whenFalse :: Link
        --   , returns :: Link
        --   }


annotationToType :: VariableMapping -> TypeAnnotation -> Deducer Type
annotationToType mapping annotation = do
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
            argType <- annotationToType mapping arg
            returningType <- annotationToType mapping returnType
            T.Function argType returningType
                |> return

        TA.Custom { typeName, args } -> do
            argTypes <- traverse (annotationToType mapping) args
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
    unboundTypes <- traverse (const generateInstancedType) variables
    unboundTypes
        |> List.zip variables
        |> Map.fromList
        |> return
