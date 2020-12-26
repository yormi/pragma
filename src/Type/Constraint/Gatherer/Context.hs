module Type.Constraint.Gatherer.Context (Context(..), gatherer) where

import Data.List.NonEmpty (NonEmpty)
import qualified Control.Monad as Monad

import AST.Identifier (DataId, TypeId)
import qualified AST.Module as M
import AST.TypeAnnotation (TypeAnnotation)
import qualified AST.TypeAnnotation as TypeAnnotation
import qualified Type.Model as T
import qualified Type.Constraint.Model as Constraint
import Type.Constraint.Gatherer.Context.Model (Context)
import qualified Type.Constraint.Gatherer.Context.Model as Context
import Type.Constraint.Gatherer.Model (Gatherer)
import qualified Type.Constraint.Gatherer.Model as Gatherer
import qualified Type.Constraint.Gatherer.TypeAnnotation as TypeAnnotation
import qualified Utils.List as List


gatherer :: M.Module -> Gatherer Context
gatherer (M.Module topLevels) = do
    contextWithTypes <-
        topLevels
            |> Monad.foldM
                ( \context topLevel ->
                    case topLevel of
                        M.SumType { M.typeName } -> do
                            case Context.defineType typeName context of
                                Just updatedContext ->
                                    return updatedContext

                                Nothing ->
                                    Gatherer.fail Gatherer.TypeNameAlreadyExists

                        _ ->
                            return context
                )
                Context.initialContext

    Gatherer.withContext contextWithTypes <|
        Monad.foldM
            ( \context topLevel ->
                case topLevel of
                    M.Function { M.functionName, M.typeAnnotation } ->
                        functionGatherer context functionName typeAnnotation

                    M.SumType { M.typeName, M.dataChoices } ->
                        sumTypeGatherer typeName dataChoices context
            )
            contextWithTypes
            topLevels


sumTypeGatherer
    :: TypeId -> NonEmpty M.DataChoice -> Context -> Gatherer Context
sumTypeGatherer identifier dataChoices context = do
    Monad.foldM
        (\resultingContext (M.DataChoice { tag, args }) -> do
            type_ <- constructorType args identifier
            Context.addConstructor tag type_ resultingContext
                |> return
        )
        context
        dataChoices


constructorType :: [TypeAnnotation] -> TypeId -> Gatherer T.Type
constructorType args typeId =
    let
        argsToConstructorType :: [T.Type] -> T.Type
        argsToConstructorType argTypes =
            case argTypes of
                [] ->
                    T.Custom typeId

                argType : rest ->
                    argsToConstructorType rest
                        |> T.FunctionType argType
                        |> T.Function
    in
    args
        |> traverse TypeAnnotation.gather
        |> map argsToConstructorType



functionGatherer :: Context -> DataId -> TypeAnnotation -> Gatherer Context
functionGatherer context functionName typeAnnotation =
    let
        hasTypeVariable =
            TypeAnnotation.extractTypeVariables
                typeAnnotation
                |> (not << List.isEmpty)

    in do
    signatureType <- TypeAnnotation.gather typeAnnotation

    type_ <-
        if hasTypeVariable then do
            returnType <- Gatherer.freshVariable
            Constraint.Generalized functionName signatureType returnType
                |> Gatherer.addConstraint

            return (T.Variable returnType)
        else
            return signatureType

    Context.addData functionName type_ context
        |> return
