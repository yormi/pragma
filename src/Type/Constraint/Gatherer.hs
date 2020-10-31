module Type.Constraint.Gatherer
    ( Constraint
    , ConstraintError(..)
    , Gatherer
    , addConstraint
    , gatherConstraints
    , eval
    , fail
    , freshVariable
    , lookupReference
    , withEnv
    ) where

import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map

import Control.Monad.Trans.RWS.CPS (RWST)
import qualified Control.Monad.Trans.RWS.CPS as RWST

import qualified AST.Expression as E
import qualified Type as T


type Constraint = (T.Type, T.Type)


--data TypeScheme
--    = TypeScheme
--        { variables :: [TypeVariable]
--        , type_ :: T.Type
--        }


type Gatherer a =
    RWST
        TypeEnv
        [Constraint]
        NextTypeVariable
        (Either ConstraintError)
        a


type TypeEnv = Map E.Identifier TypeVariable


type TypeName = String


type NextTypeVariable = TypeVariable


type TypeVariable = Int


data ConstraintError
    = TODO String
    | UnboundVariable E.Identifier
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
        initialEnv = Map.empty

        firstTypeVariable = 0
    in
    RWST.evalRWST
        gatherer
        initialEnv
        firstTypeVariable


fail :: ConstraintError -> Gatherer a
fail =
    lift << Left


lookupReference :: E.Identifier -> Gatherer TypeVariable
lookupReference identifier = do
    env <- RWST.ask
    case Map.lookup identifier env of
        Just type_ ->
            return type_

        Nothing ->
            fail <| UnboundVariable identifier


-- TODO Fail on shadowing
withEnv ::
    [(TypeName, TypeVariable)] -> Gatherer a -> Gatherer a
withEnv newEnv = do
  let scope oldEnv =
        List.foldl
            (\env (name, typeVariable) ->
                env
                    |> removeFromEnv name
                    |> extendEnv (name, typeVariable)
            )
            oldEnv
            newEnv
  RWST.local scope


removeFromEnv :: TypeName -> TypeEnv -> TypeEnv
removeFromEnv typeVariable env =
    Map.delete typeVariable env


extendEnv :: (TypeName, TypeVariable) -> TypeEnv -> TypeEnv
extendEnv (name, typeVariable) env =
    Map.insert name typeVariable env


freshVariable :: Gatherer TypeVariable
freshVariable = do
    nextTypeVariable <- RWST.get
    RWST.put <| nextTypeVariable + 1
    return nextTypeVariable


addConstraint :: T.Type -> T.Type -> Gatherer ()
addConstraint a b =
    RWST.tell [(a, b)]
