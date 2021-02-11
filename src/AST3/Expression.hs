module AST3.Expression
    ( BoolLiteral(..)
    , Case(..)
    , Definition(..)
    , Expression(..)
    , Pattern(..)
    , Value(..)
    ) where


import Data.List.NonEmpty (NonEmpty)

import Parser3.Model.Position (Position)
import Parser3.Model.Quote (Quote)
import AST3.Identifier (DataId, ReferenceId)


data Expression
    = Value Value
    | Reference ReferenceId
    | If
        { fromPosition :: Position
        , condition :: Expression
        , whenTrue :: Expression
        , whenFalse :: Expression
        }
    | LetIn
        { definitions :: NonEmpty Definition
        , body :: Expression
        }
    | CaseOf
        { element :: Expression
        , cases :: NonEmpty Case
        }
    | Lambda
        { params :: NonEmpty DataId
        , body :: Expression
        }
    | Application
        { functionName :: ReferenceId
        , args :: NonEmpty Expression
        }
    deriving (Eq, Show)


data Definition
    = SimpleDefinition DataId Expression
    deriving (Eq, Show)


data Case
    = Case
        { pattern_ :: Pattern
        , caseBody :: Expression
        }
    deriving (Eq, Show)


data Pattern
    = WildCardPattern
    | ValuePattern Value
    | IdentifierPattern DataId
    | TuplePattern Pattern Pattern
    deriving (Eq, Show)


data Value
    = Bool BoolLiteral
    | Char Quote Char
    | Float Quote Double
    | Int Quote Int
    | String Quote String
    deriving (Eq, Show)


data BoolLiteral
    = TrueLiteral Quote
    | FalseLiteral Quote
    deriving (Eq, Show)

