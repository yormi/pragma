module Check.Type.DeduceType
    ( Constraint
    , deduceType
    ) where

import AST.Module (TopLevel)
import AST.Expression (Expression)
import qualified AST.Expression as Expression
import Check.Type.ArrangeResolution (Arranged)
import Context.Model (Context)
import Parser.Model.Quote (Quote)
import Type.Model (Type, TypePlaceholder)
import Utils.NonEmpty (NonEmpty)


data Constraint
    = TypedExpression
    | If
        { condition :: (Quote, Type)
        , whenTrue :: (Quote, Type)
        , whenFalse :: (Quote, Type)
        }


-- Handle Instanciation
deduceType :: [Arranged] -> [Constraint]
deduceType resolutions =
    []
