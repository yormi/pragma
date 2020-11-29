module AST.Expression
    ( BoolLiteral(..)
    , Case(..)
    , Definition(..)
    , Expr(..)
    , Expression(..)
    , Identifier
    , Pattern(..)
    , Position(..)
    , Value(..)
    ) where


import Data.List.NonEmpty (NonEmpty)


type Identifier = String


data Expr =
    Expr
        { position :: Position
        , expression :: Expression
        }
        deriving (Eq, Show)


data Position =
    Position
        { filename :: String
        , line :: Int
        , column :: Int
        }
        deriving (Eq, Show)


data Expression
    = Value Value
    | Reference Identifier
    | If
        { condition :: Expr
        , whenTrue :: Expr
        , whenFalse :: Expr
        }
    | LetIn
        { definitions :: NonEmpty Definition
        , body :: Expr
        }
    | CaseOf
        { element :: Expr
        , cases :: NonEmpty Case
        }
    | Lambda
        { params :: NonEmpty Identifier
        , body :: Expr
        }
    | Application
        { functionName :: Identifier
        , args :: NonEmpty Expr
        }
    deriving (Eq, Show)


data Definition
    = SimpleDefinition Identifier Expr
    deriving (Eq, Show)


data Case
    = Case { pattern :: Pattern, caseBody :: Expr }
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

