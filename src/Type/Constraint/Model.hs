module Type.Constraint.Model (Constraint(..), QuotedType(..)) where

import Data.List.NonEmpty (NonEmpty)

import AST.CodeQuote (CodeQuote)
import AST.Identifier (DataId, ReferenceId)
import qualified AST.Expression as E
import qualified Type.Model as T
import Type.Constraint.Reference (Reference)


data Constraint
    = IfThenElse
        { codeQuote :: CodeQuote
        , condition :: QuotedType
        , whenTrue :: QuotedType
        , whenFalse :: QuotedType
        , placeholder :: T.TypePlaceholder
        }
    | Application
        { codeQuote :: CodeQuote
        , functionName :: ReferenceId
        , args :: NonEmpty E.QuotedExpression
        , functionReference :: T.Type
        , argTypes :: NonEmpty T.Type
        , placeholder :: T.TypePlaceholder
        }
    | Function
        { codeQuote :: CodeQuote
        , signatureType :: T.Type
        , params :: [T.Type]
        , body :: T.Type
        }
    | Definition
        { dataId :: DataId
        , type_ :: T.Type
        , placeholder :: T.TypePlaceholder
        }
    | Reference
        { reference :: Reference
        , type_ :: T.Type
        , placeholder :: T.TypePlaceholder
        }
    deriving (Eq, Show)


data QuotedType =
    QuotedType
        { codeQuote :: CodeQuote
        , quotedType :: T.Type
        }
    deriving (Eq, Show)
