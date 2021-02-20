module Context.Type
    ( Error(..)
    , asMap
    , Context
    , context
    ) where

import qualified Control.Monad as Monad
import qualified Data.Map as Map

import Parser.Model.Quote (Quote(..))
import AST.Identifier (TypeId, TypeVariableId)
import qualified AST.Module as M
import qualified Type.Model as T
import Utils.OrderedSet (OrderedSet)


newtype Context
    = Context (Map TypeId T.Kind)
        deriving (Eq, Show)


data Error
    = TypeNameAlreadyExists
        { quote :: Quote
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
                M.SumType { M.typeName, M.typeVariables } -> do
                    let aQuote = Quote "aFilePath" 1 1 1 1
                    sumType aQuote typeName typeVariables typeContext

                _ ->
                    return typeContext
        )
        initialContext



sumType
    :: Quote
    -> TypeId
    -> OrderedSet TypeVariableId
    -> Context
    -> Either Error Context
sumType quote typeId typeVariables (Context typeContext) =
    if Map.member typeId typeContext then
        TypeNameAlreadyExists quote typeId
            |> Left

    else
        typeVariables
            |> \tvs -> T.Kind tvs typeId
            |> \kind -> Map.insert typeId kind typeContext
            |> Context
            |> Right


asMap :: Context -> Map TypeId T.Kind
asMap (Context c) =
    c
