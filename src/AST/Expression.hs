module AST.Expression
    ( BoolLiteral(..)
    , Case(..)
    , Definition(..)
    , QuotedExpression(..)
    , Expression(..)
    , Pattern(..)
    , Value(..)
    ) where


import Data.List.NonEmpty (NonEmpty)

import AST.CodeQuote (CodeQuote)
import AST.Identifier (DataId, ReferenceId)


data QuotedExpression =
    QuotedExpression
        { codeQuote :: CodeQuote
        , expression :: Expression
        }
        deriving (Eq, Show)


data Expression
    = Value Value
    | Reference ReferenceId
    | If
        { condition :: QuotedExpression
        , whenTrue :: QuotedExpression
        , whenFalse :: QuotedExpression
        }
    | LetIn
        { definitions :: NonEmpty Definition
        , body :: QuotedExpression
        }
    | CaseOf
        { element :: QuotedExpression
        , cases :: NonEmpty Case
        }
    | Lambda
        { params :: NonEmpty DataId
        , body :: QuotedExpression
        }
    | Application
        { functionName :: ReferenceId
        , args :: NonEmpty QuotedExpression
        }
    deriving (Eq, Show)


data Definition
    = SimpleDefinition DataId QuotedExpression
    deriving (Eq, Show)


data Case
    = Case { pattern :: Pattern, caseBody :: QuotedExpression }
    deriving (Eq, Show)


data Pattern
    = WildCardPattern
    | ValuePattern Value
    | IdentifierPattern DataId
    | TuplePattern Pattern Pattern
    deriving (Eq, Show)


data Value
    = Bool BoolLiteral
    | Char Char
    | Float Double
    | Int Integer
    | String String
    deriving (Eq, Show)


data BoolLiteral
    = TrueLiteral
    | FalseLiteral
    deriving (Eq, Show)

