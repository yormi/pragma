module Type.Constraint.Gatherer.Model
    ( Constraint
    , ConstraintError(..)
    , Gatherer
    , addConstraint
    , gatherConstraints
    , eval
    , fail
    , freshVariable
    , lookupDataReference
    , lookupTypeVariable
    , run
    , withDataReferences
    , withTypeVariable
    ) where

import qualified Data.List as List

import Control.Monad.Trans.RWS.CPS (RWST)
import qualified Control.Monad.Trans.RWS.CPS as RWST

import qualified AST.Expression as E
import qualified AST.TypeAnnotation as TypeAnnotation
import qualified Type as T
import Type.Constraint.Model (Constraint(..))
import Type.Constraint.Gatherer.DataScope (DataScope)
import qualified Type.Constraint.Gatherer.DataScope as DataScope
import Type.Constraint.Gatherer.TypeScope (TypeScope)
import qualified Type.Constraint.Gatherer.TypeScope as TypeScope


type Gatherer a =
    RWST
        Scopes
        [Constraint]
        NextTypeVariable
        (Either ConstraintError)
        a


data Scopes
    = Scopes
        { typeScope :: TypeScope
        , dataScope :: DataScope
        }



type NextTypeVariable = TypeVariable


type TypeVariable = Int


data ConstraintError
    = TODO String
    | UnboundVariable E.Identifier
    | NotAFunction T.Type
    | TooManyParameters
        { functionType :: T.Type
        , params :: [E.Identifier]
        }
    | TooManyArguments
        { functionType :: T.Type
        , arguments :: [E.Identifier]
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
        initialScopes =
            Scopes TypeScope.initial DataScope.initial

        firstTypeVariable =
            0
    in
    RWST.evalRWST
        gatherer
        initialScopes
        firstTypeVariable


fail :: ConstraintError -> Gatherer a
fail =
    lift << Left


lookupDataReference :: E.Identifier -> Gatherer T.Type
lookupDataReference identifier = do
    scopes <- RWST.ask
    let dataEnv = dataScope scopes
    case DataScope.lookup identifier dataEnv of
        Just type_ ->
            return type_

        Nothing ->
            fail <| UnboundVariable identifier


-- TODO Fail on shadowing
withDataReferences :: [(E.Identifier, T.Type)] -> Gatherer a -> Gatherer a
withDataReferences newReferences =
    RWST.local <|
        \scopes ->
            List.foldl
                (\resultingScope (name, typeVariable) ->
                    DataScope.extend name typeVariable resultingScope
                )
                (dataScope scopes)
                newReferences
                |> \newDataScope -> scopes { dataScope = newDataScope }


--- TYPE SCOPE ---


withTypeVariable
    :: [(TypeAnnotation.Identifier, T.TypeVariable)] -> Gatherer a -> Gatherer a
withTypeVariable typeVariables =
    RWST.local <|
        \scopes ->
            List.foldl
                (\resultingScope (name, typeVariable) ->
                    TypeScope.extend name typeVariable resultingScope
                )
                (typeScope scopes)
                typeVariables
                |> \ts -> scopes { typeScope = ts }


lookupTypeVariable
    :: TypeAnnotation.Identifier -> Gatherer (Maybe T.TypeVariable)
lookupTypeVariable identifier = do
    scopes <- RWST.ask
    typeScope scopes
        |> TypeScope.lookup identifier
        |> return


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
