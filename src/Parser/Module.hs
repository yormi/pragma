module Parser.Module
    ( moduleParser
    ) where


import qualified AST.CodeQuote as CodeQuote
import AST.Identifier (DataId)
import AST.Module (DataChoice(..), Module(..), TopLevel(..))
import qualified AST.TypeAnnotation as T
import Parser.Parser (Parser, ParserError(..))
import qualified Parser.Expression as Expression
import qualified Parser.Parser as Parser
import qualified Utils.NonEmpty as NonEmpty


moduleParser :: Parser Module
moduleParser = do
    module_ <-
        Parser.oneOf
            [ sumType
            , function
            ]
            |> Parser.many
            |> map Module
    Parser.endOfFile
    return module_


sumType :: Parser TopLevel
sumType = do
    from <- Parser.position
    Parser.reserved "type"
    typeName <- Parser.typeIdentifier

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
    return <| SumType codeQuote typeName dataChoices


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
            Parser.unconsumeOnFailure <|
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
        , Parser.unconsumeOnFailure <| map (T.Custom) <| Parser.typeIdentifier
        , map (T.Variable) <| Parser.typeVariableIdentifier
        ]


