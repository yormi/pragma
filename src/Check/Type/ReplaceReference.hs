module Check.Type.ReplaceReference
    ( Expression
    , replaceReference
    ) where

import AST.Module (TopLevel)
import qualified AST.Expression as Expression
import Check.Type.Context (Context)
import Type.Model (Type, TypePlaceholder)
import Utils.NonEmpty (NonEmpty)


data Expression
    = Value
    | Placeholder TypePlaceholder
    | ContextReference Type
    | LetIn
        { definitions :: NonEmpty (TypePlaceholder, Expression.Definition)
        , body :: Expression
        }
    deriving (Eq, Show)


replaceReference :: Context -> TopLevel -> Expression
replaceReference _ _ =
    Value
