module Check.Type.Deduce.Deducer
    ( Deducer
    , Deductions
    , Error(..)
    , State(..)
    , addDeduction
    , fail
    , generateInstancedType
    , initialState
    , lookupDeduction
    , run
    ) where

import Control.Monad.Trans.Except (ExceptT)
import qualified Control.Monad.Trans.Except as Except
import qualified Control.Monad.State as State

import qualified Check.Type.Arrange as A
import Check.Type.Model.Type (Type)
import qualified Check.Type.Model.Type as T
import qualified Utils.Map as Map


type Deducer a =
    ExceptT Error (State.State State) a


data State =
    State
        { deductions :: Deductions
        , nextInstancedType :: T.InstancedType
        }


initialState :: State
initialState =
    State Map.empty (T.InstancedType 0)


type Deductions =
    Map A.Link Type


newtype Error
    = ThisIsABug String
    deriving (Eq, Show)


run :: Deducer a -> Either Error Deductions
run deducer =
    Except.runExceptT deducer
        |> flip State.runState initialState
        |> (\(either, ds) -> map (const ds) either)
        |> map deductions


addDeduction :: A.Link -> Type -> Deducer ()
addDeduction link type_ =
    State.modify
        (\state ->
            state
                |> deductions
                |> Map.insert link type_
                |> \ds -> state { deductions = ds }
        )


lookupDeduction :: A.Link -> Deducer (Maybe Type)
lookupDeduction link =
    lift <| State.gets (deductions >> Map.lookup link)


generateInstancedType :: Deducer T.InstancedType
generateInstancedType = do
    instancedType@(T.InstancedType n) <- State.gets nextInstancedType
    let next = T.InstancedType <| n + 1
    State.modify (\state -> state { nextInstancedType = next })
    return instancedType


fail :: Error -> Deducer a
fail =
    Except.throwE
