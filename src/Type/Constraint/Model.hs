module Type.Constraint.Model (Constraint(..), Element(..)) where

import Data.List.NonEmpty (NonEmpty)

import qualified AST.Expression as E
import qualified Type as T


data Constraint
    = Simple
        { newComing :: Element
        , concludedFrom :: Element
        }
    | IfThenElse
        { codeQuote :: E.CodeQuote
        , condition :: Element
        , whenTrue :: Element
        , whenFalse :: Element
        , returnType :: T.Type
        }
    | Application
        { position :: E.Position
        , functionName :: E.Identifier
        , args :: NonEmpty E.Expr
        , functionReference :: T.Type
        , argTypes :: NonEmpty T.Type
        , returnType :: T.Type
        }
    | Function
        { functionType :: T.Type
        , params :: [T.Type]
        , body :: T.Type
        }
    deriving (Eq, Show)


data Element =
    Element
        { position :: E.Position
        , expression :: E.Expression
        , type_ :: T.Type
        }
    deriving (Eq, Show)
