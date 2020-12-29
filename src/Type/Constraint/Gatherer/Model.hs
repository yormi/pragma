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
    , run
    , withContext
    , withData
    ) where

import qualified Control.Monad as Monad
import Control.Monad.Trans.RWS.CPS (RWST)
import qualified Control.Monad.Trans.RWS.CPS as RWST

import AST.Identifier (DataId, ReferenceId, TypeId)
import qualified Type.Model as T
import Type.Constraint.Model (Constraint(..))
import Type.Constraint.Context.Model (Context)
import qualified Type.Constraint.Context.Model as Context
import qualified Utils.Either as Either


type Gatherer a =
    RWST
        Context
        [Constraint]
        NextTypeVariable
        (Either ConstraintError)
        a


type NextTypeVariable = T.TypePlaceholder


data ConstraintError
    = TODO String
    | DataNameAlreadyExistsInScope
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


gatherConstraints
    :: Context -> Gatherer a -> Either ConstraintError [Constraint]
gatherConstraints context =
    run context >> map snd


eval :: Context -> Gatherer a -> Either ConstraintError a
eval context =
    run context >> map fst


run :: Context -> Gatherer a -> Either ConstraintError (a, [Constraint])
run context gatherer =
    let
        firstTypeVariable =
            T.TypePlaceholder 0
    in
    RWST.evalRWST gatherer context firstTypeVariable


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


withData :: [(DataId, T.Type)] -> Gatherer a -> Gatherer a
withData newReferences gatherer = do
    context <- RWST.ask
    let newContext =
            Monad.foldM
                (\resultingContext (name, type_) ->
                    Context.addData name type_ resultingContext
                        |> Either.mapLeft (const DataNameAlreadyExistsInScope)
                )
                context
                newReferences
    case newContext of
        Right c ->
            RWST.local (const c) gatherer

        Left e ->
            fail e


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


freshVariable :: Gatherer T.TypePlaceholder
freshVariable = do
    (T.TypePlaceholder nextTypeVariable) <- RWST.get
    let next = T.TypePlaceholder <| nextTypeVariable + 1
    RWST.put next
    return next


--- CONSTRAINT ---


addConstraint :: Constraint -> Gatherer ()
addConstraint c =
    RWST.tell [c]
