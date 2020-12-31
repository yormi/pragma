module Type.Model
    ( Kind(..)
    , Type(..)
    , FunctionType(..)
    , TypePlaceholder(..)
    , variables
    , replaceVariables
    ) where

import qualified Data.Monoid as Monoid
import qualified Data.Map as Map
import qualified Data.Set as Set

import AST.Identifier (TypeId, TypeVariableId)
import Utils.OrderedSet (OrderedSet)
import qualified Utils.Maybe as Maybe


data Kind
    = Kind
        { typeVariables :: [TypeVariableId]
        , typeId :: TypeId
        }
    deriving (Eq, Show)


data Type
    = Bool
    | Int
    | Float
    | Char
    | String
    | Function FunctionType
    | Variable TypeVariableId
    | Placeholder TypePlaceholder
    | Custom TypeId (OrderedSet Type)
    deriving (Eq, Ord, Show)


data FunctionType = FunctionType Type Type
    deriving (Eq, Ord, Show)


newtype TypePlaceholder
    = TypePlaceholder Int
    deriving (Eq, Ord, Show)


variables :: Type -> Set TypeVariableId
variables t =
    case t of
        Variable v -> do
            Set.singleton v

        Custom _ args ->
            map variables args
                |> Monoid.mconcat

        Function (FunctionType arg returning) ->
            Set.union
                (variables arg)
                (variables returning)

        _ ->
            Set.empty


replaceVariables :: Map TypeVariableId Type -> Type -> Type
replaceVariables replacements type_ =
    case type_ of
        Variable variable ->
            Map.lookup variable replacements
                |> Maybe.withDefault type_

        Custom typeId typeParams ->
            let
                replacedParams =
                    map (replaceVariables replacements) typeParams
            in
            Custom typeId replacedParams

        Function (FunctionType arg returning) ->
            let
                replacedArg =
                    replaceVariables replacements arg

                replacedReturning =
                    replaceVariables replacements returning
            in
            FunctionType replacedArg replacedReturning
                |> Function

        _ ->
            type_
