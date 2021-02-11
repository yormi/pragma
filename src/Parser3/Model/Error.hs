module Parser3.Model.Error (Error(..)) where

import Parser3.Model.Position (Position)
import Parser3.Model.Quote (Quote)


data Error
    = ThisIsABug Position String
    | EndOfFileReached

    -- COMBINATOR
    | NotTheDesiredChar Position Char
    | NonDesiredChar Position Char
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

    -- INDENTATION
    | TopLevelIndentationExpected Position
    | SameLineOrIndentedExpected Position

    -- IDENTIFIER
    | DataIdMustStartWithLowerCase Quote

    -- VALUE
    | CharExpected Position
    | BooleanExpected Position
    | IntExpected Position
    | ValueExpected Position

    -- MODULE
    | FunctionMustHaveTypeSignature Quote
    | TypeSignatureNameMismatch Quote

    deriving (Eq, Show)
