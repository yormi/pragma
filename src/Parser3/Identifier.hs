module Parser3.Identifier (reference) where

import AST3.Identifier
    ( ReferenceId
    )
import qualified AST3.Identifier as Identifier
import Parser3.Parser (Parser)
import qualified Parser3.Lexeme as Lexeme


reference :: Parser ReferenceId
reference = do
    (quote, id) <- Lexeme.identifier
    return <| Identifier.referenceId quote id
