module Check.Type.Deduce
    ( Deduced(..)
    , Error
    , deduceType
    ) where

import Control.Monad.State (StateT)
import qualified Control.Monad.State as State
import qualified Data.Map as Map

import qualified Check.Type.Arrange as A
import Parser.Model.Quote (Quote)
import Check.Type.Model (Type)
import qualified Check.Type.Model as T


type Deducer a =
    StateT Deduced (Either Error) a


type Deduced =
    Map A.Link Type


newtype Error
    = ThisIsABug String
    deriving (Eq, Show)


addDeduction :: A.Link -> Type -> Deducer ()
addDeduction link type_ =
    State.modify (Map.insert link type_)


-- ACTION


-- Handle Instanciation
deduceType :: [A.Expression] -> Either Error Deduced
deduceType arrangedExpressions =
    let
        initialState =
            Map.empty
    in
    traverse deducer arrangedExpressions
        |> flip State.execStateT initialState


deducer :: A.Expression -> Deducer ()
deducer arrangedExpression =
    case arrangedExpression of
        A.Value link (A.Int _) ->
            addDeduction link T.Int

        A.Value link (A.Float _) ->
            addDeduction link T.Float

        A.ContextReference link annotation ->
            addDeduction link type_

        -- A.Future link  ->
        --     addDeduction link type_

        -- Definition
        --   { link :: Link
        --   , futurePlaceholder :: F.Placeholder
        --   , body :: Link
        --   }
        -- OrderedIf
        --   { condition :: Link
        --   , whenTrue :: Link
        --   , whenFalse :: Link
        --   , returns :: Link
        --   }
