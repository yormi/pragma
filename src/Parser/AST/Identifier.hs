module Parser.AST.Identifier
    ( constructor
    , data_
    , reference
    , type_
    , typeVariable
    ) where

import AST.Identifier
    ( ConstructorId
    , DataId
    , ReferenceId
    , TypeId
    , TypeVariableId
    )
import qualified AST.Identifier as Identifier
import qualified Parser.Model.Error as Error
import Parser.Parser (Parser)
import qualified Parser.Parser as Parser
import qualified Parser.Lexeme as Lexeme
import Utils.Maybe as Maybe


constructor :: Parser ConstructorId
constructor = do
    (quote, id) <- Lexeme.identifier
    Identifier.constructorId quote id
        |> map return
        |> Maybe.withDefault
            (Parser.fail <| Error.ConstructorIdMustStartWithUpperCase quote )


data_ :: Parser DataId
data_ = do
    (quote, id) <- Lexeme.identifier
    Identifier.dataId quote id
        |> map return
        |> Maybe.withDefault
            (Parser.fail <| Error.DataIdMustStartWithLowerCase quote )


reference :: Parser ReferenceId
reference = do
    (quote, id) <- Lexeme.identifier
    return <| Identifier.referenceId quote id


type_ :: Parser TypeId
type_ = do
    (quote, id) <- Lexeme.identifier
    Identifier.typeId quote id
        |> map return
        |> Maybe.withDefault
            (Parser.fail <| Error.TypeIdMustStartWithUpperCase quote )


typeVariable :: Parser TypeVariableId
typeVariable = do
    (quote, id) <- Lexeme.identifier
    Identifier.typeVariableId quote id
        |> map return
        |> Maybe.withDefault
            (Parser.fail <| Error.TypeVariableIdMustStartWithLowerCase quote )
