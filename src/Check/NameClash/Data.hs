module Check.NameClash.Data
    ( check
    ) where

import AST.Module (TopLevel)
import AST.Expression (Expression)
import qualified AST.Expression as Expression
import Context.Model (Context)
import Parser.Model.Quote (Quote)
import Type.Model (Type, TypePlaceholder)
import Utils.NonEmpty (NonEmpty)


data NameClash =
    NameClash
        { existing :: Quote
        , newOne :: Quote
        }
        deriving (Eq, Show)


check :: Context -> TopLevel -> [NameClash]
check context topLevel =
    []



