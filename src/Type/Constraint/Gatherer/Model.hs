module Type.Constraint.Gatherer.Model
    ( Constraint
    , ConstraintError(..)
    , Gatherer
    , addConstraint
    , eval
    , fail
    , nextPlaceholder
    , gatherConstraints
    , lookupReference
    , run
    , withContext
    , withData
    ) where

import qualified Control.Monad as Monad
import Control.Monad.Trans.RWS.CPS (RWST)
import qualified Control.Monad.Trans.RWS.CPS as RWST
import Data.Map (Map)
import qualified Data.Map as Map

import AST.Identifier (DataId, ReferenceId)
import qualified Type.Model as T
import Type.Constraint.Model (Constraint(..))
import Type.Constraint.Context.Model (Context)
import qualified Type.Constraint.Context.Data as DataContext
import qualified Type.Constraint.Context.Constructor as ConstructorContext
import qualified Type.Constraint.Context.Model as Context
import qualified Type.Constraint.Model as Constraint
import Type.Constraint.Reference (Reference)
import qualified Type.Constraint.Reference as Reference
import qualified Utils.List as List


type Gatherer a =
    RWST
        ReferencesInScope
        [Constraint]
        NextTypeVariable
        (Either ConstraintError)
        a


type ReferencesInScope =
    Map Reference T.Type


baseReferences :: ReferencesInScope
baseReferences =
    Map.empty


type NextTypeVariable = T.TypePlaceholder


data ConstraintError
    = TODO String
    | DataNameAlreadyDefined
    | VariableNotDefined Reference
    | TooManyParameters
        { functionType :: T.Type
        , params :: [DataId]
        }
    | ShouldNotHappen String
    deriving (Eq, Show)


gatherConstraints
    :: Context -> Gatherer a -> Either ConstraintError [Constraint]
gatherConstraints context =
    run context >> map snd


eval :: Context -> Gatherer a -> Either ConstraintError a
eval context =
    run context >> map fst


fail :: ConstraintError -> Gatherer a
fail =
    lift << Left


run :: Context -> Gatherer a -> Either ConstraintError (a, [Constraint])
run context gatherer =
    let
        firstTypeVariable =
            T.TypePlaceholder 0


        references =
            Map.empty
    in do
    RWST.evalRWST
        (withContext context gatherer)
        references
        firstTypeVariable


withContext :: Context -> Gatherer a -> Gatherer a
withContext context gatherer =
    let
        dataReferenceIds =
            context
                |> Context.data_
                |> DataContext.asMap
                |> Map.keys
                |> map Reference.fromDataId

        constructorReferenceIds =
            context
                |> Context.constructor
                |> ConstructorContext.asMap
                |> Map.keys
                |> map Reference.fromConstructorId
    in do
    data_ <-
        context
            |> Context.data_
            |> DataContext.asMap
            |> Map.toList
            |> traverse
                (\(dataId, type_) ->
                    let
                        reference =
                            Reference.fromDataId dataId
                    in do
                    p <- nextPlaceholder
                    Constraint.Reference reference type_ p
                        |> addConstraint

                    return (reference, p)
                )

    constructors <-
        context
            |> Context.constructor
            |> ConstructorContext.asMap
            |> Map.toList
            |> traverse
                (\(constructorId, type_) ->
                    let
                        reference =
                            Reference.fromConstructorId constructorId
                    in do
                    p <- nextPlaceholder
                    Constraint.Reference reference type_ p
                        |> addConstraint

                    return (reference, p)
                )


    let referenceContext =
            data_ ++ constructors
                |> List.foldl
                    (\result (reference, placeholder) ->
                        Map.insert reference (T.Placeholder placeholder) result
                    )
                    baseReferences
    RWST.local (Map.union referenceContext) gatherer


lookupReference :: ReferenceId -> Gatherer T.Type
lookupReference referenceId =
    let
        reference =
            Reference.fromReferenceId referenceId
    in do
    context <- RWST.ask
    case Map.lookup reference context of
        Just type_ ->
            return type_

        Nothing ->
            fail <| VariableNotDefined reference


withData :: [(DataId, T.Type)] -> Gatherer a -> Gatherer a
withData newReferences gatherer = do
    context <- RWST.ask
    let newContext =
            Monad.foldM
                (\resultingContext (name, type_) ->
                    let
                        reference =
                            Reference.fromDataId name
                    in
                    if Map.member reference resultingContext then
                        Left DataNameAlreadyDefined

                    else
                        Map.insert reference type_ resultingContext
                            |> Right
                )
                context
                newReferences
    case newContext of
        Right c ->
            RWST.local (const c) gatherer

        Left e ->
            fail e


--- TYPE VARIABLE ---


nextPlaceholder :: Gatherer T.TypePlaceholder
nextPlaceholder = do
    (T.TypePlaceholder nextTypeVariable) <- RWST.get
    let next = T.TypePlaceholder <| nextTypeVariable + 1
    RWST.put next
    return next


--- CONSTRAINT ---


addConstraint :: Constraint -> Gatherer ()
addConstraint c =
    RWST.tell [c]
