module Type.Constraint.Model (Constraint(..), QuotedType(..)) where

import Data.List.NonEmpty (NonEmpty)

import AST.CodeQuote (CodeQuote)
import AST.Identifier (ConstructorId, DataId, ReferenceId, TypeVariableId)
import qualified AST.Expression as E
import qualified Type.Model as T
import Utils.OrderedSet (OrderedSet)


data Constraint
    = IfThenElse
        { codeQuote :: CodeQuote
        , condition :: QuotedType
        , whenTrue :: QuotedType
        , whenFalse :: QuotedType
        , returnType :: T.TypePlaceholder
        }
    | Application
        { codeQuote :: CodeQuote
        , functionName :: ReferenceId
        , args :: NonEmpty E.QuotedExpression
        , functionReference :: T.Type
        , argTypes :: NonEmpty T.Type
        , returnType :: T.TypePlaceholder
        }
    | Function
        { codeQuote :: CodeQuote
        , signatureType :: T.Type
        , params :: [T.Type]
        , body :: T.Type
        }
    | Definition
        { dataId :: DataId
        , actualType :: T.Type
        , returnType :: T.TypePlaceholder
        }
    | TopLevelFunction
        { codeQuote :: CodeQuote
        , dataId :: DataId
        , typeVariables :: OrderedSet TypeVariableId
        , actualType :: T.Type
        , returnType :: T.TypePlaceholder

        , signatureType :: T.Type
        , params :: [T.Type]
        , body :: T.Type
        }
    | Constructor
        { constructorId :: ConstructorId
        , typeVariables :: OrderedSet TypeVariableId
        , actualType :: T.Type
        , returnType :: T.TypePlaceholder
        }
    deriving (Eq, Show)


data QuotedType =
    QuotedType
        { codeQuote :: CodeQuote
        , type_ :: T.Type
        }
    deriving (Eq, Show)
