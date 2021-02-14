module Parser3.AST.Module
    ( moduleParser
    ) where

import AST3.Identifier (DataId, TypeVariableId, typeVariableQuote)
-- import qualified AST3.Expression as Expression
-- import AST3.Module (DataChoice(..), Field(..), Module(..), TopLevel(..))
import AST3.Module (DataChoice(..), Module(..), TopLevel(..))
import qualified AST3.TypeAnnotation as Annotation
import Parser3.Parser (Parser)
import qualified Parser3.Combinator as C
import qualified Parser3.AST.Expression as Expression
import qualified Parser3.AST.Identifier as Identifier
import qualified Parser3.Indentation as Indentation
import qualified Parser3.Lexeme as Lexeme
import qualified Parser3.Model.Error as Error
import qualified Parser3.Model.Quote as Quote
import qualified Parser3.Parser as Parser
import qualified Utils.List as List
import qualified Utils.Maybe as Maybe
-- import Utils.NonEmpty (NonEmpty)
import qualified Utils.NonEmpty as NonEmpty
import Utils.OrderedSet (OrderedSet)
import qualified Utils.OrderedSet as OrderedSet


moduleParser :: Parser Module
moduleParser = do
    module_ <-
        C.oneOf
            -- [ record
            -- [ Parser.unconsumeOnFailure record
            [ sumType
            , function
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


sumType :: Parser TopLevel
sumType = do
    from <- Parser.getPosition
    _ <- Lexeme.reserved "type"
    typeName <- Identifier.type_

    typeVariables <- typeVariableDeclaration

    _ <- Lexeme.operator "="
    firstChoice <- dataChoice

    otherChoices <-
        C.many <| do
            _ <- Lexeme.operator "|"
            dataChoice

    let dataChoices = NonEmpty.build firstChoice otherChoices
    return <| SumType from typeName typeVariables dataChoices


typeVariableDeclaration :: Parser (OrderedSet TypeVariableId)
typeVariableDeclaration = do
    C.someSpace
    from <- Parser.getPosition

    typeVariables <- C.many Identifier.typeVariable

    let areThereDupplicates = List.unique typeVariables /= typeVariables
    if areThereDupplicates then
        let
            quote =
                typeVariables
                    |> map typeVariableQuote
                    |> map Quote.to
                    |> List.last
                    |> Maybe.withDefault from
                    |> Quote.fromPositions from
        in
        Parser.fail <| Error.TypeVariableIdMustBeUniqueInDeclaration quote

    else
        return <| OrderedSet.fromList typeVariables


dataChoice :: Parser DataChoice
dataChoice = do
    tag <- Identifier.constructor
    args <- C.many <| typeAnnotationParser
    return <| DataChoice tag args


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
