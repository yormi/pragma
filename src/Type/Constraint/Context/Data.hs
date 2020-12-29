module Type.Constraint.Context.Data
    ( Error(..)
    , Context
    , addData
    , asMap
    , context
    , lookup
    ) where

import qualified Control.Monad as Monad
import Data.Map (Map)
import qualified Data.Map as Map

import AST.Identifier (DataId)
import qualified AST.Module as M
import AST.TypeAnnotation (TypeAnnotation)
import qualified Type.Constraint.Gatherer.TypeAnnotation as TypeAnnotation
import qualified Type.Model as T


data Error
    = DataNameAlreadyExistsInScope
    deriving (Eq, Show)


data Context
    = Context (Map DataId T.Type)
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
        )
        initialContext
        topLevels


function :: DataId -> TypeAnnotation -> Context -> Either Error Context
function functionName typeAnnotation =
    let
        signatureType =
            TypeAnnotation.toType typeAnnotation
    in
    addData functionName signatureType


addData :: DataId -> T.Type -> Context -> Either Error Context
addData dataId type_ (Context c) =
    if Map.member dataId c then
        Left DataNameAlreadyExistsInScope
    else
        Map.insert dataId type_ c
            |> Context
            |> Right


lookup :: DataId -> Context -> Maybe T.Type
lookup dataId (Context c) =
    Map.lookup dataId c


asMap :: Context -> Map DataId T.Type
asMap (Context c) =
    c
