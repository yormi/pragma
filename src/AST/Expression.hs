module AST.Expression
    ( BoolLiteral(..)
    , Case(..)
    , Definition(..)
    , QuotedExpression(..)
    , Expression(..)
    , Identifier
    , Pattern(..)
    , Value(..)
    ) where


import Data.List.NonEmpty (NonEmpty)

import AST.CodeQuote (CodeQuote)


type Identifier = String


data QuotedExpression =
    QuotedExpression
        { codeQuote :: CodeQuote
        , expression :: Expression
        }
        deriving (Eq, Show)


data Expression
    = Value Value
    | Reference Identifier
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
        { params :: NonEmpty Identifier
        , body :: QuotedExpression
        }
    | Application
        { functionName :: Identifier
        , args :: NonEmpty QuotedExpression
        }
    deriving (Eq, Show)


data Definition
    = SimpleDefinition Identifier QuotedExpression
    deriving (Eq, Show)


data Case
    = Case { pattern :: Pattern, caseBody :: QuotedExpression }
    deriving (Eq, Show)


data Pattern
    = WildCardPattern
    | ValuePattern Value
    | IdentifierPattern Identifier
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

