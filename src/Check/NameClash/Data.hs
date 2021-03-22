module Check.NameClash.Data
    ( check
    ) where

import AST.Module (TopLevel)
import Context.Model (Context)
import Parser.Model.Quote (Quote)


data NameClash =
    NameClash
        { existing :: Quote
        , newOne :: Quote
        }
        deriving (Eq, Show)


check :: Context -> TopLevel -> [NameClash]
check _ _ =
    []



