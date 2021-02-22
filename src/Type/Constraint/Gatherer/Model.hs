module Type.Constraint.Gatherer.Model
    ( Constraint
    , ConstraintError(..)
    , Gatherer
    , addConstraint
    , eval
    , fail
    , nextPlaceholder
    , recordingGeneratedPlaceholder
    , gatherConstraints
    , lookupReference
    , referencesInScope
    , run
    , withContext
    , withData
    ) where

import qualified Control.Monad as Monad
import Control.Monad.Trans.RWS.CPS (RWST)
import qualified Control.Monad.Trans.RWS.CPS as RWST
import qualified Data.Map as Map
import qualified Data.Set as Set

import AST.Identifier (ReferenceId)
import qualified Type.Model as T
import Type.Constraint.Model (Constraint(..))
import Context.Model (Context)
import qualified Context.Data as DataContext
import qualified Context.Constructor as ConstructorContext
import qualified Context.Model as Context
import qualified Type.Constraint.Model as Constraint
import Type.Constraint.Reference (Reference)
import qualified Type.Constraint.Reference as Reference
import qualified Utils.List as List
import qualified Utils.Tuple as Tuple


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
        , params :: [Reference]
        }
    | CyclicDependenciesInLetDefinitions
    | ShouldNotHappen String
    deriving (Eq, Show)



-- EXECUTE


gatherConstraints
    :: Context -> Gatherer a -> Either ConstraintError [Constraint]
gatherConstraints context =
    run context >> map Tuple.second


eval :: Context -> Gatherer a -> Either ConstraintError a
eval context =
    run context >> map Tuple.first


run :: Context -> Gatherer a -> Either ConstraintError (a, [Constraint])
run context gatherer =
    let
        firstTypePlaceholder =
            T.TypePlaceholder 0


        references =
            Map.empty
    in do
    RWST.evalRWST
        (withContext context gatherer)
        references
        firstTypePlaceholder



-- TOP-LEVEL CONTEXT


withContext :: Context -> Gatherer a -> Gatherer a
withContext context gatherer = do
    data_ <-
        context
            |> Context.data_
            |> DataContext.asMap
            |> Map.toList
            |> traverse
                (\(dataId, typeInfo) ->
                    let
                        reference =
                            Reference.fromDataId dataId
                    in do
                    p <- nextPlaceholder
                    case typeInfo of
                        DataContext.TopLevel typeAnnotation ->
                            Constraint.TopLevelDefinition
                                reference
                                typeAnnotation
                                p
                                |> addConstraint

                        DataContext.LetDefinition _ ->
                            return ()

                    return (reference, p)
                )

    constructors <-
        context
            |> Context.constructor
            |> ConstructorContext.asMap
            |> Map.toList
            |> traverse
                (\(constructorId, typeAnnotation) ->
                    let
                        reference =
                            Reference.fromConstructorId constructorId
                    in do
                    p <- nextPlaceholder
                    Constraint.TopLevelDefinition reference typeAnnotation p
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



-- FAILURE


fail :: ConstraintError -> Gatherer a
fail =
    lift << Left



-- REFERENCE


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


referencesInScope :: Gatherer (Set Reference)
referencesInScope = do
    context <- RWST.ask
    Map.keys context
        |> Set.fromList
        |> return



-- IN-FUNCTION DEFINITION


withData :: Map Reference T.Type -> Gatherer a -> Gatherer a
withData newReferences gatherer = do
    context <- RWST.ask
    let newContext =
            Monad.foldM
                (\resultingContext (reference, type_) ->
                    if Map.member reference resultingContext then
                        Left DataNameAlreadyDefined

                    else
                        Map.insert reference type_ resultingContext
                            |> Right
                )
                context
                (Map.toList newReferences)
    case newContext of
        Right c ->
            RWST.local (const c) gatherer

        Left e ->
            fail e



-- PLACEHOLDER


nextPlaceholder :: Gatherer T.TypePlaceholder
nextPlaceholder = do
    (T.TypePlaceholder placeholderNumber) <- RWST.get
    let next = T.TypePlaceholder <| placeholderNumber + 1
    RWST.put next
    return next


recordingGeneratedPlaceholder
    :: Gatherer a -> Gatherer (Set T.TypePlaceholder, a)
recordingGeneratedPlaceholder gatherer =
    let
        readNextPlaceholderNumber = do
            (T.TypePlaceholder placeholderNumber) <- RWST.get
            return placeholderNumber
    in do
    fromPlaceholder <- readNextPlaceholderNumber
    result <- gatherer
    nextPlaceholderAfter <- readNextPlaceholderNumber

    let generated =
            List.range fromPlaceholder nextPlaceholderAfter
                |> map T.TypePlaceholder
                |> Set.fromList
    return (generated, result)


-- CONSTRAINT


addConstraint :: Constraint -> Gatherer ()
addConstraint c =
    RWST.tell [c]
