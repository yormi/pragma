module Parser.AST.Module
    ( moduleParser
    ) where

import AST.Identifier (DataId, TypeVariableId, typeVariableQuote)
import qualified AST.Identifier as Identifier
-- import qualified AST.Expression as Expression
-- import AST.Module (DataChoice(..), Field(..), Module(..), TopLevel(..))
import AST.Module (DataChoice(..), Module(..), TopLevel(..))
import qualified AST.TypeAnnotation as Annotation
import Parser.Parser (Parser)
import qualified Parser.Combinator as C
import qualified Parser.AST.Expression as Expression
import qualified Parser.AST.Identifier as Identifier
import qualified Parser.Indentation as Indentation
import qualified Parser.Lexeme as Lexeme
import qualified Parser.Model.Error as Error
import qualified Parser.Model.Quote as Quote
import qualified Parser.Parser as Parser
import qualified Utils.List as List
import qualified Utils.Maybe as Maybe
-- import Utils.NonEmpty (NonEmpty)
import qualified Utils.NonEmpty as NonEmpty
import Utils.OrderedSet (OrderedSet)
import qualified Utils.OrderedSet as OrderedSet
import qualified Utils.String as String


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
--     let quote = Quote.fromPositions from to
--     return <| Record quote typeName typeVariables fields
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
--     let quote = Quote.fromPositions from to
--     Field quote fieldName annotation
--         |> return
-- 
-- 
-- orFail :: ParserError -> Parser a -> Parser a
-- orFail =
--     Parser.catchUncaughtError
-- 
-- 
-- orFailWithPosition
--     :: (Quote.Position -> ParserError) -> Parser a -> Parser a
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
    args <- C.until
        (do
            _ <- Identifier.data_
            Lexeme.operator ":"
        )
        typeAnnotationParser
    return <| DataChoice tag args


function :: Parser TopLevel
function = do
    C.someSpace
    from <- Parser.getPosition
    Indentation.topLevel

    typeLine <-
        Parser.until
            (do
                _ <- C.char '\n'
                _ <- C.many <| C.oneOf [ C.char ' ', C.char '\t' ]
                _ <- Identifier.data_
                _ <- C.many <| C.anyCharBut [ '=', '\n' ]
                C.char '='
            )
            (C.maybe typeLineParser)


    -- typeLine <- C.maybe typeLineParser

    functionName <- Identifier.data_
    params <- C.many Identifier.data_
    _ <- Lexeme.operator "="

    body <- Indentation.withPositionReference Expression.expressionParser
    to <- Parser.getPosition -- FIXME

    let quote = Quote.fromPositions from to

    case typeLine of
        Just (typeLineName, type_) ->
            let
                signatureName =
                    Identifier.formatDataId typeLineName

                definitionName =
                    Identifier.formatDataId functionName
            in
            if signatureName == definitionName then do
                return <| Function quote type_ functionName params body
            else do
                Error.TypeSignatureNameMismatch quote
                    |> Parser.fail

        Nothing -> do
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
