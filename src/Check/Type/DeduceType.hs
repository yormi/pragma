module Check.Type.DeduceType
    ( Constraint
    , deduceType
    ) where

import qualified Check.Type.Arrange as A
import Parser.Model.Quote (Quote)
import Check.Type.Model (Type)


data Constraint
    = TypedExpression
    | If
        { condition :: (Quote, Type)
        , whenTrue :: (Quote, Type)
        , whenFalse :: (Quote, Type)
        }


-- Handle Instanciation
deduceType :: [A.Expression] -> [Constraint]
deduceType resolutions =
    []
