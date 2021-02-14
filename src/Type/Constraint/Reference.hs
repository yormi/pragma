module Type.Constraint.Reference
    ( Reference
    , fromConstructorId
    , fromDataId
    , fromReferenceId
    , asString
    ) where

import AST.Identifier (ConstructorId, DataId, ReferenceId)
import qualified AST.Identifier as Identifier


newtype Reference
    = Reference String
    deriving (Eq, Ord, Show)


asString :: Reference -> String
asString (Reference str) =
    str


fromConstructorId :: ConstructorId -> Reference
fromConstructorId id =
    Reference <| Identifier.formatConstructorId id


fromDataId :: DataId -> Reference
fromDataId id =
    Reference <| Identifier.formatDataId id


fromReferenceId :: ReferenceId -> Reference
fromReferenceId id =
    Reference <| Identifier.formatReferenceId id
