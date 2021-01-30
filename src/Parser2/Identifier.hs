module Parser2.Identifier (reference) where

import AST.Identifier
    ( ConstructorId
    , DataId
    , ReferenceId
    , TypeId
    , TypeVariableId
    )
import qualified AST.Identifier as Identifier
import Parser2.Parser (QuotedParser)
import qualified Parser2.Parser as P
import qualified Parser2.Combinator as C


reference :: QuotedParser ReferenceId
reference =
    C.identifier
        |> P.mapResult Identifier.referenceId
