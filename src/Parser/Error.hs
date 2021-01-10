module Parser.Error
    ( FieldError(..)
    , ParserError(..)
    , RecordError(..)
    , TypeAliasError(..)
    ) where


import Data.Aeson (FromJSON, ToJSON)

import AST.CodeQuote (CodeQuote, Position(..))
import AST.Identifier (DataId)


data ParserError
    = MismatchInProcess
    | RawError
    | SumTypeConstructorMustStartWithUpper CodeQuote
    | DataNameMustStartWithLowerCase CodeQuote
    | TypeNameMustStartWithUpperCase CodeQuote
    | TypeVariableMustStartWithLowerCase CodeQuote
    | TypeSignatureNameMismatch CodeQuote DataId DataId
    | FunctionMustHaveTypeSignature CodeQuote
    | FieldInvalid FieldError Position
    | RecordInvalid RecordError Position
    | TypeAliasInvalid TypeAliasError Position
    deriving (Eq, Generic, Show, FromJSON, ToJSON)


data RecordError
    = TrailingCharacter
    | ExtraComma
    deriving (Eq, Generic, Show, FromJSON, ToJSON)


data FieldError
    = DefinitionMustUseColon
    | MustHaveTypeAnnotation
    deriving (Eq, Generic, Show, FromJSON, ToJSON)


data TypeAliasError
    = TypeNameInvalid
    | TypeVariableInvalid
    deriving (Eq, Generic, Show, FromJSON, ToJSON)


