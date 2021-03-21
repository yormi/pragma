module Check.Type.Check
    ( TypeError
    , check
    ) where

import qualified AST.Expression as Expression
import Check.Type.Constraint (Constraint)
import Context.Model (Context)
import Parser.Model.Quote (Quote)
import Type.Model (Type, TypePlaceholder)
import Utils.NonEmpty (NonEmpty)


data TypeError
    = TypeError
    deriving (Eq, Show)


check :: Constraint -> Maybe TypeError
check expression =
    Nothing
