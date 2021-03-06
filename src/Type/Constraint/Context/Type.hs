module Type.Constraint.Context.Type
    ( Error(..)
    , asMap
    , Context
    , context
    ) where

import qualified Control.Monad as Monad
import qualified Data.Map as Map

import AST.CodeQuote (CodeQuote)
import AST.Identifier (TypeId, TypeVariableId)
import qualified AST.Module as M
import qualified Type.Model as T


newtype Context
    = Context (Map TypeId T.Kind)
        deriving (Eq, Show)


data Error
    = TypeNameAlreadyExists
        { codeQuote :: CodeQuote
        , typeId :: TypeId
        }
        deriving (Eq, Show)


initialContext :: Context
initialContext =
    Context Map.empty


context :: [M.TopLevel] -> Either Error Context
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
    -> Context
    -> Either Error Context
sumType codeQuote typeId typeVariables (Context typeContext) =
    if Map.member typeId typeContext then
        TypeNameAlreadyExists codeQuote typeId
            |> Left

    else
        Map.insert typeId (T.Kind typeVariables typeId) typeContext
            |> Context
            |> Right


asMap :: Context -> Map TypeId T.Kind
asMap (Context c) =
    c
