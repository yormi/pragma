module Parser3.Error (Error(..)) where

import Parser3.Position (Position)
import Parser3.Quote (Quote)


data Error
    = ThisIsABug Position String
    | EndOfFileReached

    | NotTheDesiredChar Position Char
    | SpaceExpected Position Char
    | StringExpected Position
    | AtLeastOneExpected Position
    | IdentifierExpected Position
    | InvalidCharactersInIdentifier [ (Position, Char) ]

    | IdentifierCantBeAReservedWord Quote String
    | ReservedWordExpected Quote String
    | NotAReservedWord Quote String
    | OperatorExpected Position String

    | ArgumentExpected Position
    | BooleanExpected Position
    | ExpressionExpected Position
    | IntExpected Position
    | ValueExpected Position


    deriving (Eq, Show)
