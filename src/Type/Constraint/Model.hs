module Type.Constraint.Model (Constraint(..), QuotedType(..)) where

import Data.List.NonEmpty (NonEmpty)

import AST.Identifier (ReferenceId)
import AST.TypeAnnotation (TypeAnnotation)
import qualified AST.Expression as E
import Parser.Model.Quote (Quote)
import qualified Type.Model as T
import Type.Constraint.Reference (Reference)


data Constraint
    = IfThenElse
        { quote :: Quote
        , condition :: QuotedType
        , whenTrue :: QuotedType
        , whenFalse :: QuotedType
        , placeholder :: T.TypePlaceholder
        }
    | Application
        { quote :: Quote
        , functionName :: ReferenceId
        , args :: NonEmpty E.Expression
        , functionReference :: T.Type
        , argTypes :: NonEmpty T.Type
        , placeholder :: T.TypePlaceholder
        }
    | Function
        { quote :: Quote
        , signatureType :: TypeAnnotation
        , params :: [T.Type]
        , body :: T.Type
        }
    | TopLevelDefinition
        { reference :: Reference
        , typeAnnotation :: TypeAnnotation
        , placeholder :: T.TypePlaceholder
        }
    | LetDefinition
        { reference :: Reference
        , type_ :: T.Type
        , placeholder :: T.TypePlaceholder
        , generated :: Set T.TypePlaceholder
        }
    deriving (Eq, Show)


data QuotedType =
    QuotedType
        { quote :: Quote
        , quotedType :: T.Type
        }
    deriving (Eq, Show)
