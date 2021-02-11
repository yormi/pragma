module Parser3.Identifier
    ( data_
    , reference
    ) where

import AST3.Identifier
    ( DataId
    , ReferenceId
    )
import qualified AST3.Identifier as Identifier
import qualified Parser3.Model.Error as Error
import Parser3.Parser (Parser)
import qualified Parser3.Parser as Parser
import qualified Parser3.Lexeme as Lexeme
import Utils.Maybe as Maybe


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
