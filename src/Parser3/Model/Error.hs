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
    | SameLineExpected Position
    | SameLineOrIndentedExpected Position

    -- IDENTIFIER
    | ConstructorIdMustStartWithUpperCase Quote
    | DataIdMustStartWithLowerCase Quote
    | TypeIdMustStartWithUpperCase Quote
    | TypeVariableIdMustStartWithLowerCase Quote

    -- VALUE
    | CharExpected Position
    | BooleanExpected Position
    | IntExpected Position
    | StringMustBeOnSingleLine Quote

    -- MODULE
    | FunctionMustHaveTypeSignature Quote
    | TypeSignatureNameMismatch Quote
    | TypeVariableIdMustBeUniqueInDeclaration Quote

    deriving (Eq, Show)
