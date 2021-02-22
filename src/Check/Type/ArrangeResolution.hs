module Check.Type.ArrangeResolution
    ( Arranged
    , arrange
    ) where

import AST.Module (TopLevel)
import AST.Expression (Expression)
import qualified AST.Expression as Expression
import Check.Type.ReplaceReference as ReplaceReference
import Context.Model (Context)
import Parser.Model.Quote (Quote)
import Type.Model (Type, TypePlaceholder)
import Utils.NonEmpty (NonEmpty)


newtype LinkPlaceholder =
    LinkPlaceholder Int
        deriving (Eq, Show)


-- Flatten the Let..In
data Arranged
    = Ordered
    | Definition TypePlaceholder ReplaceReference.Expression
    | OrderedIf
        { condition :: LinkPlaceholder
        , whenTrue :: LinkPlaceholder
        , whenFalse :: LinkPlaceholder
        , returns :: LinkPlaceholder
        }


arrange :: ReplaceReference.Expression -> [Arranged]
arrange expression =
    [Ordered]
