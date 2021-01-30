module Parser2.Error (Error(..)) where

import Parser2.Model (Position, Quote)


data Error
    = ThisIsABug Position String
    | EndOfFileReached
    | StringExpected Position
    | SpaceExpected Quote Char
    | InvalidCharactersInIdentifier [ (Position, Char) ]
    | IdentifierCantBeAReservedWord Quote String
    | ReservedWordExpected Quote String
    | NotAReservedWord Quote String
    | OperatorExpected Quote String

    | ExpressionExpected
    deriving (Eq, Show)
