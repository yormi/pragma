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
    , run
    , withEnv
    ) where

import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map

import Control.Monad.Trans.RWS.CPS (RWST)
import qualified Control.Monad.Trans.RWS.CPS as RWST

import qualified AST.Expression as E
import qualified Type as T
import Type.Constraint.Model (Constraint(..))


type Gatherer a =
    RWST
        TypeEnv
        [Constraint]
        NextTypeVariable
        (Either ConstraintError)
        a


type TypeEnv = Map E.Identifier T.Type


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


lookupReference :: E.Identifier -> Gatherer T.Type
lookupReference identifier = do
    env <- RWST.ask
    case Map.lookup identifier env of
        Just type_ ->
            return type_

        Nothing ->
            fail <| UnboundVariable identifier


-- TODO Fail on shadowing
withEnv :: [(E.Identifier, T.Type)] -> Gatherer a -> Gatherer a
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


removeFromEnv :: E.Identifier -> TypeEnv -> TypeEnv
removeFromEnv identifier env =
    Map.delete identifier env


extendEnv :: (E.Identifier, T.Type) -> TypeEnv -> TypeEnv
extendEnv (identifier, type_) env =
    Map.insert identifier type_ env


freshVariable :: Gatherer T.Type
freshVariable = do
    nextTypeVariable <- RWST.get
    RWST.put <| nextTypeVariable + 1
    return <| T.Variable nextTypeVariable


addConstraint :: Constraint -> Gatherer ()
addConstraint c =
    RWST.tell [c]
