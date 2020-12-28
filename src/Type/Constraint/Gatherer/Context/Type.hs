module Type.Constraint.Gatherer.Context.Type
    ( Error(..)
    , TypeContext
    , context
    ) where

import qualified Control.Monad as Monad
import Data.Map (Map)
import qualified Data.Map as Map

import AST.CodeQuote (CodeQuote)
import AST.Identifier (TypeId, TypeVariableId)
import qualified AST.Module as M
import qualified Type.Model as T


data Error
    = TypeNameAlreadyExists
        { codeQuote :: CodeQuote
        , typeId :: TypeId
        }
        deriving (Eq, Show)


newtype TypeContext
    = TypeContext (Map TypeId T.Kind)
        deriving (Eq, Show)


initialContext :: TypeContext
initialContext =
    TypeContext Map.empty


context :: [M.TopLevel] -> Either Error TypeContext
context =
    Monad.foldM
        ( \typeContext topLevel ->
            case topLevel of
                M.SumType { M.codeQuote, M.typeName, M.typeVariables } -> do
                    sumType codeQuote typeName typeVariables typeContext

                _ ->
                    return typeContext
        )
        initialContext



sumType
    :: CodeQuote
    -> TypeId
    -> [TypeVariableId]
    -> TypeContext
    -> Either Error TypeContext
sumType codeQuote typeId typeVariables (TypeContext typeContext) =
    if Map.member typeId typeContext then
        TypeNameAlreadyExists codeQuote typeId
            |> Left

    else
        Map.insert typeId (T.Kind typeVariables typeId) typeContext
            |> TypeContext
            |> Right
