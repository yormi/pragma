module Type.Constraint.Gatherer.Context.Model
    ( Context(..)
    , addConstructor
    , addData
    , defineType
    , initialContext
    , lookupReference
    , isTypeDefined
    ) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import AST.Identifier (ConstructorId, DataId, TypeId, ReferenceId)
import qualified AST.Identifier as Identifier
import qualified Type.Model as T


data Context
    = Context
        { constructor :: Map ConstructorId T.Type
        , type_ :: Set TypeId
        , data_ :: Map DataId T.Type
        }
        deriving (Eq, Show)


addConstructor :: ConstructorId -> T.Type -> Context -> Context
addConstructor constructorId type_ context =
    constructor context
        |> Map.insert constructorId type_
        |> \s -> context { constructor = s }


addData :: DataId -> T.Type -> Context -> Context
addData dataId type_ context =
    data_ context
        |> Map.insert dataId type_
        |> \s -> context { data_ = s }


defineType :: TypeId -> Context -> Maybe Context
defineType typeId context =
    let
        typeContext =
            type_ context
    in
    if Set.member typeId typeContext then
        Nothing
    else
        typeContext
            |> Set.insert typeId
            |> \s -> context { type_ = s }
            |> Just


lookupReference :: ReferenceId -> Context -> Maybe T.Type
lookupReference identifier context =
    let
        specializedId =
            Identifier.dataOrConstructor identifier
    in
    case specializedId of
        Right dataId ->
            Map.lookup dataId (data_ context)

        Left constructorId ->
            Map.lookup constructorId (constructor context)


isTypeDefined :: TypeId -> Context -> Bool
isTypeDefined identifier context =
    Set.member identifier (type_ context)


initialContext :: Context
initialContext =
    Context
        Map.empty
        Set.empty
        Map.empty


