module Context.Data
    ( Error(..)
    , Context
    , DataTypeInfo(..)
    , addLetDefinition
    , asMap
    , context
    , lookup
    ) where

import qualified Control.Monad as Monad
import qualified Data.Map as Map

import AST.Identifier (DataId)
import qualified AST.Module as M
import AST.TypeAnnotation (TypeAnnotation)
import qualified Type.Model as T


newtype Context
    = Context (Map DataId DataTypeInfo)
    deriving (Eq, Show)


data DataTypeInfo
    = TopLevel TypeAnnotation
    | LetDefinition T.TypePlaceholder
    deriving (Eq, Show)


data Error
    = DataNameAlreadyExistsInScope
    deriving (Eq, Show)


initialContext :: Context
initialContext =
    Context Map.empty


context :: [M.TopLevel] -> Either Error Context
context topLevels =
    Monad.foldM
        ( \resultingContext topLevel ->
            case topLevel of
                M.Function { M.functionName, M.typeAnnotation } ->
                    function functionName typeAnnotation resultingContext

                M.SumType {} -> do
                    return resultingContext

                M.Record {} -> do
                    return resultingContext
        )
        initialContext
        topLevels


function :: DataId -> TypeAnnotation -> Context -> Either Error Context
function dataId typeAnnotation =
    TopLevel typeAnnotation
        |> addData dataId


addLetDefinition
    :: DataId -> T.TypePlaceholder -> Context -> Either Error Context
addLetDefinition dataId placeholder =
    LetDefinition placeholder
        |> addData dataId


addData :: DataId -> DataTypeInfo -> Context -> Either Error Context
addData dataId typeInfo (Context c) =
    if Map.member dataId c then
        Left DataNameAlreadyExistsInScope
    else
        Map.insert dataId typeInfo c
            |> Context
            |> Right


lookup :: DataId -> Context -> Maybe DataTypeInfo
lookup dataId (Context c) =
    Map.lookup dataId c


asMap :: Context -> Map DataId DataTypeInfo
asMap (Context c) =
    c
