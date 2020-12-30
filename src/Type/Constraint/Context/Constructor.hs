module Type.Constraint.Context.Constructor
    ( Context
    , asMap
    , context
    , lookup
    ) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty

import AST.CodeQuote (CodeQuote)
import AST.Identifier (ConstructorId, TypeId, TypeVariableId)
import qualified AST.Module as M
import AST.TypeAnnotation (TypeAnnotation)
import qualified Type.Model as T
import qualified Type.Constraint.Gatherer.TypeAnnotation as TypeAnnotation
import qualified Type.Constraint.Context.Type as Type
import qualified Utils.List as List
import Utils.OrderedSet (OrderedSet)


newtype Context
    = Context (Map ConstructorId T.Type)
        deriving (Eq, Show)

data Error
    = ConstructorNameAlreadyExists
        { codeQuote :: CodeQuote
        , typeId :: TypeId
        }
        deriving (Eq, Show)


initialContext :: Context
initialContext =
    Context Map.empty


lookup :: ConstructorId -> Context -> Maybe T.Type
lookup id (Context c)=
    Map.lookup id c


context :: Type.Context -> [M.TopLevel] -> Context
context typeContext topLevels =
    List.foldl
        ( \resultingContext topLevel ->
            case topLevel of
                M.SumType { M.typeName, M.typeVariables, M.dataChoices } -> do
                    sumType typeName typeVariables dataChoices resultingContext

                M.Function {} ->
                    resultingContext

        )
        initialContext
        topLevels


sumType
    :: TypeId
    -> OrderedSet TypeVariableId
    -> NonEmpty M.DataChoice
    -> Context
    -> Context
sumType typeId typeVariableIds dataChoices (Context c) =
    let
        resultingType =
            T.Custom typeId (map T.Variable typeVariableIds)
    in
    dataChoices
        |> NonEmpty.toList
        |> List.foldl
            (\resultingContext (M.DataChoice { tag, args }) ->
                let
                    type_ =
                        constructorType typeVariableIds args resultingType
                in
                Map.insert tag type_ resultingContext
            )
            c
        |> Context


constructorType
    :: OrderedSet TypeVariableId
    -> [TypeAnnotation]
    -> T.Type
    -> T.Type
constructorType typeVariableIds args resultingType =
    let
        argsToConstructorType :: [T.Type] -> T.Type
        argsToConstructorType argTypes =
            case argTypes of
                [] ->
                    resultingType

                argType : rest ->
                    argsToConstructorType rest
                        |> T.FunctionType argType
                        |> T.Function
    in
    args
        |> map TypeAnnotation.gather
        |> map (\(_, type_) -> type_)
        |> argsToConstructorType


asMap :: Context -> Map ConstructorId T.Type
asMap (Context c) =
    c
