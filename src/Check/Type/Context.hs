module Check.Type.Context
    ( Context
    , build
    , lookupReference
    ) where

import qualified Control.Monad as Monad
import Control.Monad.State (State)
import qualified Control.Monad.State as State
import qualified Data.Set as Set
import qualified GHC.Err as GHC

import AST.Identifier (TypeVariableId)
import qualified AST.Identifier as Identifier
import qualified AST.Module as M
import AST.TypeAnnotation (TypeAnnotation)
import qualified AST.TypeAnnotation as TA
import Check.Type.Model (Type)
import qualified Check.Type.Model as T
import qualified Utils.List as List
import qualified Utils.Map as Map
import Utils.NonEmpty (NonEmpty)
import qualified Utils.NonEmpty as NonEmpty
import Utils.OrderedSet (OrderedSet)
import qualified Utils.OrderedSet as OrderedSet


type Context =
    Map Reference Type


type Reference =
    String


type Builder a =
    State NextInstancedType a


type NextInstancedType =
    T.InstancedType


type VariableMapping =
    Map TypeVariableId T.InstancedType


lookupReference :: Reference -> Context -> Maybe  T.Type
lookupReference =
    Map.lookup


build :: M.Module -> Context
build (M.Module topLevels) =
    let
        initialState =
            T.InstancedType 0
    in
    topLevels
        |> Monad.mapM dataInTopLevel
        |> flip State.evalState initialState
        |> Map.unions


dataInTopLevel :: M.TopLevel -> Builder Context
dataInTopLevel topLevel =
    case topLevel of
        M.Function { typeAnnotation, functionName } -> do
            mapping <- variableMapping typeAnnotation
            type_ <- annotationToType mapping typeAnnotation
            ( Identifier.formatDataId functionName, type_ )
                |> List.singleton
                |> Map.fromList
                |> return

        M.SumType { typeName, typeVariables, dataChoices } ->
            sumType typeName typeVariables dataChoices


sumType
    :: Identifier.TypeId
    -> OrderedSet TypeVariableId
    -> NonEmpty M.DataChoice
    -> Builder Context
sumType typeId typeVariableIds dataChoices =
    let
        finalAnnotation =
            typeVariableIds
                |> OrderedSet.toList
                |> map TA.Variable
                |> TA.Custom typeId
    in do
    mapping <- variableMapping finalAnnotation
    dataChoices
        |> NonEmpty.toList
        |> traverse
            (\M.DataChoice { tag, args } ->
                let
                    annotation =
                        constructorAnnotation args finalAnnotation
                in do
                type_ <- annotationToType mapping annotation
                let constructorName = Identifier.formatConstructorId tag
                return (constructorName, type_)
            )
        |> map Map.fromList



constructorAnnotation :: [TypeAnnotation] -> TypeAnnotation -> TypeAnnotation
constructorAnnotation args finalAnnotation =
    let
        functionFromArgs argAnnotations =
            case argAnnotations of
                [] ->
                    finalAnnotation

                argAnnotation : rest ->
                    functionFromArgs rest
                        |> TA.Function argAnnotation
    in
    functionFromArgs args


annotationToType :: VariableMapping -> TypeAnnotation -> Builder Type
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


variableMapping :: TypeAnnotation -> Builder VariableMapping
variableMapping annotation =
    let
        variables =
            TA.extractTypeVariables annotation
                |> Set.toList
    in do
    unboundTypes <- traverse (const nextInstancedType) variables
    unboundTypes
        |> List.zip variables
        |> Map.fromList
        |> return


nextInstancedType :: Builder T.InstancedType
nextInstancedType = do
    instancedType@(T.InstancedType n) <- State.get
    let next = T.InstancedType <| n + 1
    State.put next
    return instancedType
