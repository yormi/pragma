module Type.Constraint.Gatherer.Model
    ( Constraint
    , ConstraintError(..)
    , Gatherer
    , addConstraint
    , eval
    , fail
    , freshVariable
    , gatherConstraints
    , lookupReference
    , checkIfTypeDefined
    , run
    , withContext
    , withConstructors
    , withData
    ) where

import Control.Monad.Trans.RWS.CPS (RWST)
import qualified Control.Monad.Trans.RWS.CPS as RWST
import qualified Data.List as List

import AST.Identifier (ConstructorId, DataId, ReferenceId, TypeId)
import qualified Type.Model as T
import Type.Constraint.Model (Constraint(..))
import Type.Constraint.Gatherer.Context.Model (Context)
import qualified Type.Constraint.Gatherer.Context.Model as Context


type Gatherer a =
    RWST
        Context
        [Constraint]
        NextTypeVariable
        (Either ConstraintError)
        a


type NextTypeVariable = TypeVariable


type TypeVariable = Int


data ConstraintError
    = TODO String
    | VariableNotDefined ReferenceId
    | TypeNameAlreadyExists
    | TypeNotDefined TypeId
    | NotAFunction T.Type
    | TooManyParameters
        { functionType :: T.Type
        , params :: [DataId]
        }
    | TooManyArguments
        { functionType :: T.Type
        , arguments :: [DataId]
        }
    | ShouldNotHappen String
    deriving (Eq, Show)


gatherConstraints :: Gatherer a -> Either ConstraintError [Constraint]
gatherConstraints =
    run >> map snd


eval :: Gatherer a -> Either ConstraintError a
eval =
    run >> map fst


run :: Gatherer a -> Either ConstraintError (a, [Constraint])
run gatherer =
    let
        firstTypeVariable =
            0
    in
    RWST.evalRWST gatherer Context.initialContext firstTypeVariable


fail :: ConstraintError -> Gatherer a
fail =
    lift << Left


withContext :: Context -> Gatherer a -> Gatherer a
withContext context =
    RWST.local (const context)


lookupReference :: ReferenceId -> Gatherer T.Type
lookupReference identifier = do
    context <- RWST.ask
    case Context.lookupReference identifier context of
        Just type_ ->
            return type_

        Nothing ->
            fail <| VariableNotDefined identifier


checkIfTypeDefined :: TypeId -> Gatherer ()
checkIfTypeDefined identifier = do
    context <- RWST.ask
    if Context.isTypeDefined identifier context then
        return ()

    else
        fail <| TypeNotDefined identifier


-- TODO Fail on shadowing
withData :: [(DataId, T.Type)] -> Gatherer a -> Gatherer a
withData newReferences =
    RWST.local <|
        \context ->
            List.foldl
                (\resultingContext (name, type_) ->
                    Context.addData name type_ resultingContext
                )
                context
                newReferences


withConstructors :: [(ConstructorId, T.Type)] -> Gatherer a -> Gatherer a
withConstructors newReferences =
    RWST.local <|
        \context ->
            List.foldl
                (\resultingContext (name, type_) ->
                    Context.addConstructor name type_ resultingContext
                )
                context
                newReferences


--- TYPE SCOPE ---


--withTypeVariable
--    :: [(TypeAnnotation.Identifier, T.TypeVariable)] -> Gatherer a -> Gatherer a
-- withTypeVariable typeVariables =
--     RWST.local <|
--         \scopes ->
--             List.foldl
--                 (\resultingScope (name, typeVariable) ->
--                     TypeContext.extend name typeVariable resultingScope
--                 )
--                 (type_ scopes)
--                 typeVariables
--                 |> \ts -> scopes { type_ = ts }


-- lookupTypeVariable
--     :: TypeAnnotation.Identifier -> Gatherer (Maybe T.TypeVariable)
-- lookupTypeVariable identifier = do
--     scopes <- RWST.ask
--     type_ scopes
--         |> TypeContext.lookup identifier
--         |> return


--- TYPE VARIABLE ---


freshVariable :: Gatherer T.TypeVariable
freshVariable = do
    nextTypeVariable <- RWST.get
    RWST.put <| nextTypeVariable + 1
    return nextTypeVariable


--- CONSTRAINT ---


addConstraint :: Constraint -> Gatherer ()
addConstraint c =
    RWST.tell [c]
