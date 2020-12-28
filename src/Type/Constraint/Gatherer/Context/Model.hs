module Type.Constraint.Gatherer.Context.Model
    ( Context(..)
    , Error(..)
    , context
    , addConstructor
    , addData
    , lookupReference
    ) where

import Data.Map (Map)
import qualified Data.Map as Map

import AST.Identifier (ConstructorId, DataId, ReferenceId)
import qualified AST.Identifier as Identifier
import qualified AST.Module as M
import qualified Type.Model as T
import Type.Constraint.Gatherer.Context.Type (TypeContext)
import qualified Type.Constraint.Gatherer.Context.Type as Type
import qualified Utils.Either as Either


data Error
    = TypeError Type.Error
    | ConstructorError
    | DataError
    deriving (Eq, Show)


data Context
    = Context
        { type_ :: TypeContext
        , constructor :: Map ConstructorId T.Type
        , data_ :: Map DataId T.Type
        }
        deriving (Eq, Show)


context :: [M.TopLevel] -> Either Error Context
context topLevels =
    Type.context topLevels
        |> Either.mapLeft TypeError
        |> map (\t -> Context t Map.empty Map.empty)


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
