module Type.Constraint.Model (Constraint(..), QuotedType(..)) where

import Data.List.NonEmpty (NonEmpty)

import AST.CodeQuote (CodeQuote)
import qualified AST.Expression as E
import qualified Type as T


data Constraint
    = IfThenElse
        { codeQuote :: CodeQuote
        , condition :: QuotedType
        , whenTrue :: QuotedType
        , whenFalse :: QuotedType
        , returnType :: T.TypeVariable
        }
    | Application
        { codeQuote :: CodeQuote
        , functionName :: E.Identifier
        , args :: NonEmpty E.QuotedExpression
        , functionReference :: T.Type
        , argTypes :: NonEmpty T.Type
        , returnType :: T.TypeVariable
        }
    | Function
        { codeQuote :: CodeQuote
        , signatureType :: T.Type
        , params :: [T.Type]
        , body :: T.Type
        }
    | Generalized
        { actualType :: T.Type
        , returnType :: T.TypeVariable
        }
    deriving (Eq, Show)


data QuotedType =
    QuotedType
        { codeQuote :: CodeQuote
        , type_ :: T.Type
        }
    deriving (Eq, Show)
