module Type.Constraint.Model (Constraint(..), Element(..)) where

import Data.List.NonEmpty (NonEmpty)

import AST.CodeQuote (CodeQuote, Position)
import qualified AST.Expression as E
import qualified Type as T


data Constraint
    = Simple
        { newComing :: Element
        , concludedFrom :: Element
        }
    | IfThenElse
        { codeQuote :: CodeQuote
        , condition :: Element
        , whenTrue :: Element
        , whenFalse :: Element
        , returnType :: T.Type
        }
    | Application
        { codeQuote :: CodeQuote
        , functionName :: E.Identifier
        , args :: NonEmpty E.Expr
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


data Element =
    Element
        { position :: Position
        , expression :: E.Expression
        , type_ :: T.Type
        }
    deriving (Eq, Show)
