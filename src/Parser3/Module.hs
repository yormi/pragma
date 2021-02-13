module Parser3.Module
    ( moduleParser
    ) where

import AST3.Identifier (DataId)
-- import qualified AST3.Expression as Expression
-- import AST3.Module (DataChoice(..), Field(..), Module(..), TopLevel(..))
import AST3.Module (Module(..), TopLevel(..))
import qualified AST3.TypeAnnotation as Annotation
import Parser3.Parser (Parser)
import qualified Parser3.Combinator as C
import qualified Parser3.Expression as Expression
import qualified Parser3.Identifier as Identifier
import qualified Parser3.Indentation as Indentation
import qualified Parser3.Lexeme as Lexeme
import qualified Parser3.Model.Error as Error
import qualified Parser3.Model.Quote as Quote
import qualified Parser3.Parser as Parser
-- import Utils.NonEmpty (NonEmpty)
-- import qualified Utils.NonEmpty as NonEmpty

moduleParser :: Parser Module
moduleParser = do
    module_ <-
        C.oneOf
            [ -- record
            -- [ Parser.unconsumeOnFailure record
            -- , sumType
             function
            ]
            |> C.many
            |> map Module
    C.endOfFile
    return module_


-- record :: Parser TopLevel
-- record = do
--     from <- Parser.getPosition
--     Lexeme.reserved "type"
--     Lexeme.reserved "alias"
-- 
--     typeName <-
--         Identifier.type_
--             |> Parser.unconsumeOnFailure
--             |> orFailWithPosition (TypeAliasInvalid TypeNameInvalid)
-- 
--     typeVariables <-
--         C.manyUntil
--             (Lexeme.operator "=")
--             (Identifier.typeVariable
--                 |> orFailWithPosition (TypeAliasInvalid TypeVariableInvalid)
--             )
-- 
--     fields <- recordDefinition
-- 
--     C.oneOf
--         [ Indentation.topLevel
--         , C.endOfFile
--         ]
--         |> orFailWithPosition (RecordInvalid TrailingCharacter)
-- 
--     to <- Parser.endPosition
--     let codeQuote = Quote.fromPositions from to
--     return <| Record codeQuote typeName typeVariables fields
-- 
-- 
-- recordDefinition :: Parser (NonEmpty Field)
-- recordDefinition = do
--     Lexeme.operator "{"
--     firstField <- field
-- 
--     otherFields <-
--         C.many <| do
--             position <- Parser.getPosition
--             Lexeme.operator ","
--             field
--                 |> orFail (RecordInvalid ExtraComma position)
--     Lexeme.operator "}"
-- 
--     NonEmpty.build firstField otherFields
--         |> return
-- 
-- 
-- field :: Parser Field
-- field = do
--     from <- Parser.getPosition
-- 
--     fieldName <- Identifier.data_
-- 
--     Lexeme.operator ":"
--         |> orFail (FieldInvalid DefinitionMustUseColon from)
-- 
--     annotation <-
--         typeAnnotationParser
--             |> orFail (FieldInvalid MustHaveTypeAnnotation from)
-- 
--     to <- Parser.endPosition
-- 
--     let codeQuote = Quote.fromPositions from to
--     Field codeQuote fieldName annotation
--         |> return
-- 
-- 
-- orFail :: ParserError -> Parser a -> Parser a
-- orFail =
--     Parser.catchUncaughtError
-- 
-- 
-- orFailWithPosition
--     :: (CodeQuote.Position -> ParserError) -> Parser a -> Parser a
-- orFailWithPosition errorBuilder parser = do
--     position <- Parser.position
--     let error = errorBuilder position
--     orFail error parser
-- 
-- 
-- sumType :: Parser TopLevel
-- sumType = do
--     from <- Parser.position
--     Parser.reserved "type"
--     typeName <- Parser.typeIdentifier
-- 
--     typeVariables <- Parser.many Parser.typeVariableIdentifier
-- 
--     Parser.reservedOperator "="
--     firstChoice <- dataChoice
-- 
--     otherChoices <-
--         Parser.many <| do
--             Parser.reservedOperator "|"
--             dataChoice
-- 
--     to <- Parser.position
--     let codeQuote = CodeQuote.fromPositions from to
--     let dataChoices =
--             NonEmpty.build firstChoice otherChoices
--     return <| SumType codeQuote typeName typeVariables dataChoices
-- 
-- 
-- dataChoice :: Parser DataChoice
-- dataChoice = do
--     Parser.withPositionReference <| do
--         from <- Parser.position
--         tag <- Parser.constructorIdentifier
-- 
--         args <-
--             Parser.many <| do
--                 Parser.sameLineOrIndented
--                 typeAnnotationParser
--         to <- Parser.position
-- 
--         let codeQuote = CodeQuote.fromPositions from to
--         return <| DataChoice codeQuote tag args
-- 
-- 
function :: Parser TopLevel
function = do
    from <- Parser.getPosition
    Indentation.topLevel

    typeLine <- C.maybe typeLineParser

    functionName <- Identifier.data_
    params <- C.many Identifier.data_
    _ <- Lexeme.operator "="

    body <- Indentation.withPositionReference Expression.expressionParser
    to <- Parser.getPosition

    let quote = Quote.fromPositions from to

    case typeLine of
        Just (typeLineName, type_) ->
            if typeLineName == functionName then
                return <| Function quote type_ functionName params body
            else
                Error.TypeSignatureNameMismatch quote
                    |> Parser.fail

        Nothing ->
            Error.FunctionMustHaveTypeSignature quote
                |> Parser.fail


typeLineParser :: Parser (DataId, Annotation.TypeAnnotation)
typeLineParser = do
    functionName <- Identifier.data_
    _ <- Lexeme.operator ":"
    type_ <- typeAnnotationParser
    return (functionName, type_)


typeAnnotationParser :: Parser Annotation.TypeAnnotation
typeAnnotationParser =
    let
        functionTypeParser =
            Parser.unconsumeOnFailure <|
                do -- careful not to start with typeParser.
                   -- It creates an infinite loop =/
                    arg <- simpleTypeParser
                    _ <- Lexeme.operator "->"
                    returnType <- typeAnnotationParser
                    return <| Annotation.Function arg returnType
    in
    C.oneOf [ functionTypeParser, simpleTypeParser ]


simpleTypeParser :: Parser Annotation.TypeAnnotation
simpleTypeParser =
    C.oneOf
        [ map (const Annotation.Bool) <| Lexeme.reserved "Bool"
        , map (const Annotation.Int) <| Lexeme.reserved "Int"
        , map (const Annotation.Float) <| Lexeme.reserved "Float"
        , map (const Annotation.Char) <| Lexeme.reserved "Char"
        , map (const Annotation.String) <| Lexeme.reserved "String"

        , Lexeme.parenthesized typeAnnotationParser

        , Parser.unconsumeOnFailure <|
            Indentation.withPositionReference <| do
                id <- Identifier.type_
                args <-
                    C.many <| do
                        Indentation.sameLine
                        simpleTypeParser
                Annotation.Custom id args
                    |> return

        , Parser.unconsumeOnFailure <| map Annotation.Variable <|
            Identifier.typeVariable
        ]
