module Type.Constraint.Gatherer.TypeScope
    ( TypeScope
    , extend
    , initial
    , lookup
    ) where

import Data.Map (Map)
import qualified Data.Map as Map

import qualified AST.TypeAnnotation as TypeAnnotation
import qualified Type as T


newtype TypeScope =
    TypeScope
        (Map TypeAnnotation.Identifier T.TypeVariable)


initial :: TypeScope
initial =
    TypeScope Map.empty


extend :: TypeAnnotation.Identifier -> T.TypeVariable -> TypeScope -> TypeScope
extend identifier typeVariable (TypeScope scope) =
    Map.insert identifier typeVariable scope
        |> TypeScope


lookup :: TypeAnnotation.Identifier -> TypeScope -> Maybe T.TypeVariable
lookup identifier (TypeScope scope) =
    Map.lookup identifier scope
