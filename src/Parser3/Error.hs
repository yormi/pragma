module Parser3.Error (Error(..)) where

import Parser3.Position (Position)
import Parser3.Quote (Quote)


data Error
    = ThisIsABug Position String
    | EndOfFileReached

    -- COMBINATOR
    | NotTheDesiredChar Position Char
    | SpaceExpected Position Char
    | StringExpected Position
    | AtLeastOneExpected Position
    | IdentifierExpected Position
    | InvalidCharactersInIdentifier [ (Position, Char) ]

    -- LEXEME
    | IdentifierCantBeAReservedWord Quote String
    | ReservedWordExpected Quote String
    | NotAReservedWord Quote String
    | OperatorExpected Position String

    -- VALUE
    | CharExpected Position
    | BooleanExpected Position
    | IntExpected Position
    | ValueExpected Position

    -- EXPRESSION
    | ExpressionExpected Position
    | ArgumentExpected Position

    deriving (Eq, Show)
