module AST.Expression
    ( Argument(..)
    , BoolLiteral(..)
    , Case(..)
    , Definition(..)
    , Expr(..)
    , Identifier
    , Pattern(..)
    , Value(..)
    ) where


import Data.List.NonEmpty (NonEmpty)


type Identifier = String


data Expr
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
        , args :: NonEmpty Argument
        }
    deriving (Eq, Show)


data Argument
    = ValueArgument Value
    | ReferenceArgument Identifier
    | ExpressionArgument Expr
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

