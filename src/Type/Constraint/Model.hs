module Type.Constraint.Model (Constraint(..), QuotedType(..)) where

import Data.List.NonEmpty (NonEmpty)

import AST.CodeQuote (CodeQuote)
import qualified AST.Expression as E
import qualified Type as T


data Constraint
    = Simple
        { newComing :: QuotedType
        , concludedFrom :: QuotedType
        }
    | IfThenElse
        { codeQuote :: CodeQuote
        , condition :: QuotedType
        , whenTrue :: QuotedType
        , whenFalse :: QuotedType
        , returnType :: T.Type
        }
    | Application
        { codeQuote :: CodeQuote
        , functionName :: E.Identifier
        , args :: NonEmpty E.QuotedExpression
        , functionReference :: T.Type
        , argTypes :: NonEmpty T.Type
        , returnType :: T.Type
        }
    | Function
        { codeQuote :: CodeQuote
        , signatureType :: T.Type
        , params :: [T.Type]
        , body :: T.Type
        }
    deriving (Eq, Show)


data QuotedType =
    QuotedType
        { codeQuote :: CodeQuote
        , type_ :: T.Type
        }
    deriving (Eq, Show)
