module Parser3.Module
    ( moduleParser
    ) where

import Data.List.NonEmpty (NonEmpty)

import AST3.Identifier (DataId)
import qualified AST3.Expression as Expression
import AST3.Module (DataChoice(..), Field(..), Module(..), TopLevel(..))
import qualified AST.TypeAnnotation as T
import Parser3.Parser (Parser)
import qualified Parser3.Combinator as C
import qualified Parser3.Expression as Expression
import qualified Parser3.Identifier as Identifier
import qualified Parser3.Indentation as Indentation
import qualified Parser3.Lexeme as Lexeme
import qualified Parser3.Model.Error as Error
import qualified Parser3.Model.Quote as Quote
import qualified Parser3.Parser as Parser
import qualified Utils.NonEmpty as NonEmpty

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
--     from <- Parser.position
--     Parser.reserved "type"
--     Parser.reserved "alias"
-- 
--     typeName <-
--         Parser.typeIdentifier
--             |> Parser.unconsumeOnFailure
--             |> orFailWithPosition (TypeAliasInvalid TypeNameInvalid)
-- 
--     typeVariables <-
--         Parser.manyUntil
--             (Parser.reservedOperator "=")
--             (Parser.typeVariableIdentifier
--                 |> orFailWithPosition (TypeAliasInvalid TypeVariableInvalid)
--             )
-- 
--     fields <- recordDefinition
-- 
--     Parser.oneOf
--         [ Parser.topLevel
--         , Parser.endOfFile
--         ]
--         |> orFailWithPosition (RecordInvalid TrailingCharacter)
-- 
--     to <- Parser.endPosition
--     let codeQuote = CodeQuote.fromPositions from to
--     return <| Record codeQuote typeName typeVariables fields
-- 
-- 
-- recordDefinition :: Parser (NonEmpty Field)
-- recordDefinition = do
--     Parser.reservedOperator "{"
--     firstField <- field
-- 
--     otherFields <-
--         Parser.many <| do
--             position <- Parser.position
--             Parser.reservedOperator ","
--             field
--                 |> orFail (RecordInvalid ExtraComma position)
--     Parser.reservedOperator "}"
-- 
--     NonEmpty.build firstField otherFields
--         |> return
-- 
-- 
-- field :: Parser Field
-- field = do
--     from <- Parser.position
-- 
--     fieldName <- Parser.dataIdentifier
-- 
--     Parser.reservedOperator ":"
--         |> orFail (FieldInvalid DefinitionMustUseColon from)
-- 
--     annotation <-
--         typeAnnotationParser
--             |> orFail (FieldInvalid MustHaveTypeAnnotation from)
-- 
--     to <- Parser.endPosition
-- 
--     let codeQuote = CodeQuote.fromPositions from to
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

    let typeLine = Nothing
    -- typeLine <-
    --     C.maybe typeLineParser

    functionName <- Identifier.data_
    params <- C.many Identifier.data_
    _ <- Lexeme.operator "="

    --body <- Identation.withPositionReference Expression.expressionParser
    to <- Parser.getPosition

    let quote = Quote.fromPositions from to

    let body =
            Expression.Int quote 3
                |> Expression.Value

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
-- 
-- 
-- typeLineParser :: Parser (DataId, T.TypeAnnotation)
-- typeLineParser = do
--     functionName <- Parser.dataIdentifier
--     Parser.reservedOperator ":"
--     type_ <- typeAnnotationParser
--     return (functionName, type_)
-- 
-- 
-- typeAnnotationParser :: Parser T.TypeAnnotation
-- typeAnnotationParser =
--     let
--         functionTypeParser =
--             Parser.unconsumeOnFailure <|
--                 do -- careful not to start with typeParser.
--                    -- It creates an infinite loop =/
--                     arg <- simpleTypeParser
--                     Parser.reservedOperator "->"
--                     returnType <- typeAnnotationParser
--                     return <| T.Function arg returnType
--     in
--     Parser.oneOf [ functionTypeParser, simpleTypeParser ]
-- 
-- 
-- simpleTypeParser :: Parser T.TypeAnnotation
-- simpleTypeParser =
--     Parser.oneOf
--         [ map (const T.Bool) <| Parser.reserved "Bool"
--         , map (const T.Int) <| Parser.reserved "Int"
--         , map (const T.Float) <| Parser.reserved "Float"
--         , map (const T.Char) <| Parser.reserved "Char"
--         , map (const T.String) <| Parser.reserved "String"
--         , do
--             Parser.reservedOperator "("
--             t <- typeAnnotationParser
--             Parser.reservedOperator ")"
--             return t
-- 
--         , Parser.unconsumeOnFailure <|
--             Parser.withPositionReference <| do
--                 id <- Parser.typeIdentifier
--                 args <-
--                     Parser.many <| do
--                         Parser.sameLine
--                         simpleTypeParser
--                 T.Custom id args
--                     |> return
-- 
--         , Parser.unconsumeOnFailure <| map (T.Variable) <|
--             Parser.typeVariableIdentifier
--         ]
