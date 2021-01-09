module Parser.Module
    ( moduleParser
    ) where

import Data.List.NonEmpty (NonEmpty)

import qualified AST.CodeQuote as CodeQuote
import AST.Identifier (DataId)
import AST.Module (DataChoice(..), Field(..), Module(..), TopLevel(..))
import qualified AST.TypeAnnotation as T
import Parser.Parser
    ( FieldError(..)
    , Parser
    , ParserError(..)
    , RecordError(..)
    , TypeAliasError(..)
    )
import qualified Parser.Expression as Expression
import qualified Parser.Parser as Parser
import qualified Utils.NonEmpty as NonEmpty

import qualified Text.Parsec as P
import qualified Parser.Debug as Debug


moduleParser :: Parser Module
moduleParser = do
    module_ <-
        Parser.oneOf
            [ record
            -- [ Parser.unconsumeOnFailure_ record
            -- , sumType
            -- , function
            ]
            |> Parser.many
            |> map Module
    Parser.endOfFile
    return module_


record :: Parser TopLevel
record = do
    from <- Parser.position
    Parser.reserved "type"
    Parser.reserved "alias"

    typeName <-
        Parser.typeIdentifier
            |> Parser.unconsumeOnFailure_
            |> orFailWithPosition (TypeAliasInvalid TypeNameInvalid)

    typeVariables <-
        Parser.typeVariableIdentifier
            |> Parser.unconsumeOnFailure_
            -- |> orFailWithPosition (TypeAliasInvalid TypeVariableInvalid)
            |> Parser.manyUntil (Parser.reservedOperator "=")

    fields <- recordDefinition

    Parser.topLevel
        |> orFailWithPosition (RecordInvalid TrailingCharacter)

    to <- Parser.endPosition
    let codeQuote = CodeQuote.fromPositions from to
    return <| Record codeQuote typeName typeVariables fields


recordDefinition :: Parser (NonEmpty Field)
recordDefinition = do
    Parser.reservedOperator "{"
    firstField <- field
    otherFields <-
        Parser.many <| do
            position <- Parser.position
            Parser.reservedOperator ","
            field
                |> orFail (RecordInvalid ExtraComma position)
    Parser.reservedOperator "}"

    NonEmpty.build firstField otherFields
        |> return


field :: Parser Field
field = do
    from <- Parser.position

    fieldName <- Parser.dataIdentifier

    Parser.reservedOperator ":"
        |> orFail (FieldInvalid DefinitionMustUseColon from)

    annotation <-
        typeAnnotationParser
            |> Parser.catchRaw (FieldInvalid MustHaveTypeAnnotation from)

    to <- Parser.endPosition

    let codeQuote = CodeQuote.fromPositions from to
    Field codeQuote fieldName annotation
        |> return


orFail :: ParserError -> Parser a -> Parser a
orFail error parser = do
    result <- P.optionMaybe parser
    case result of
        Just x ->
            return x
        Nothing ->
            Parser.fail error


orFailWithPosition
    :: (CodeQuote.Position -> ParserError) -> Parser a -> Parser a
orFailWithPosition errorBuilder parser = do
    position <- Parser.position
    let error = errorBuilder position
    orFail error parser


sumType :: Parser TopLevel
sumType = do
    from <- Parser.position
    Parser.reserved "type"
    typeName <- Parser.typeIdentifier

    typeVariables <- Parser.many Parser.typeVariableIdentifier

    Parser.reservedOperator "="
    firstChoice <- dataChoice

    otherChoices <-
        Parser.many <| do
            Parser.reservedOperator "|"
            dataChoice

    to <- Parser.position
    let codeQuote = CodeQuote.fromPositions from to
    let dataChoices =
            NonEmpty.build firstChoice otherChoices
    return <| SumType codeQuote typeName typeVariables dataChoices


dataChoice :: Parser DataChoice
dataChoice = do
    Parser.withPositionReference <| do
        from <- Parser.position
        tag <- Parser.constructorIdentifier

        args <-
            Parser.many <| do
                Parser.sameLineOrIndented
                typeAnnotationParser
        to <- Parser.position

        let codeQuote = CodeQuote.fromPositions from to
        return <| DataChoice codeQuote tag args


function :: Parser TopLevel
function = do
    from <- Parser.position
    Parser.topLevel

    typeLine <-
        Parser.maybe <| typeLineParser

    functionName <- Parser.dataIdentifier
    params <- Parser.many Parser.dataIdentifier
    Parser.reservedOperator "="

    body <- Parser.withPositionReference Expression.expressionParser
    to <- Parser.position

    let codeQuote = CodeQuote.fromPositions from to

    case typeLine of
        Just (typeLineName, type_) ->
            if typeLineName == functionName then
                return <| Function codeQuote type_ functionName params body
            else
                TypeSignatureNameMismatch codeQuote typeLineName functionName
                    |> Parser.fail

        Nothing ->
            FunctionMustHaveTypeSignature codeQuote
                |> Parser.fail


typeLineParser :: Parser (DataId, T.TypeAnnotation)
typeLineParser = do
    functionName <- Parser.dataIdentifier
    Parser.reservedOperator ":"
    type_ <- typeAnnotationParser
    return (functionName, type_)


typeAnnotationParser :: Parser T.TypeAnnotation
typeAnnotationParser =
    let
        functionTypeParser =
            Parser.unconsumeOnFailure_ <|
                do -- careful not to start with typeParser.
                   -- It creates an infinite loop =/
                    arg <- simpleTypeParser
                    Parser.reservedOperator "->"
                    returnType <- typeAnnotationParser
                    return <| T.Function arg returnType
    in
    Parser.oneOf [ functionTypeParser, simpleTypeParser ]


simpleTypeParser :: Parser T.TypeAnnotation
simpleTypeParser =
    Parser.oneOf
        [ map (const T.Bool) <| Parser.reserved "Bool"
        , map (const T.Int) <| Parser.reserved "Int"
        , map (const T.Float) <| Parser.reserved "Float"
        , map (const T.Char) <| Parser.reserved "Char"
        , map (const T.String) <| Parser.reserved "String"
        , do
            Parser.reservedOperator "("
            t <- typeAnnotationParser
            Parser.reservedOperator ")"
            return t

        , Parser.unconsumeOnFailure_ <|
            Parser.withPositionReference <| do
                id <- Parser.typeIdentifier
                args <-
                    Parser.many <| do
                        Parser.sameLine
                        simpleTypeParser
                T.Custom id args
                    |> return

        , Parser.unconsumeOnFailure_ <| map (T.Variable) <|
            Parser.typeVariableIdentifier
        ]


