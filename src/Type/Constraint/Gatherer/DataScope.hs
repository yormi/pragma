module Type.Constraint.Gatherer.DataScope
    ( DataScope
    , extend
    , initial
    , lookup
    ) where

import Data.Map (Map)
import qualified Data.Map as Map

import qualified AST.Expression as E
import qualified Type as T


type DataScope = Map E.Identifier T.Type


initial :: DataScope
initial =
    Map.empty


extend :: E.Identifier -> T.Type -> DataScope -> DataScope
extend identifier type_ env =
    Map.insert identifier type_ env


lookup :: E.Identifier -> DataScope -> Maybe T.Type
lookup =
    Map.lookup
