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
import Utils.OrderedSet (OrderedSet)
import qualified Utils.OrderedSet as OrderedSet


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
    -> OrderedSet TypeVariableId
    -> Context
    -> Either Error Context
sumType codeQuote typeId typeVariables (Context typeContext) =
    if Map.member typeId typeContext then
        TypeNameAlreadyExists codeQuote typeId
            |> Left

    else
        OrderedSet.toList typeVariables
            |> \tvs -> T.Kind tvs typeId
            |> \kind -> Map.insert typeId kind typeContext
            |> Context
            |> Right


asMap :: Context -> Map TypeId T.Kind
asMap (Context c) =
    c
