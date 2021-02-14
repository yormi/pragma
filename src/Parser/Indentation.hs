module Parser.Indentation
    ( topLevel
    , sameLine
    , sameLineOrIndented
    , withPositionReference
    )
    where


import qualified Parser.Model.Error as Error
import qualified Parser.Model.Position as Position
import Parser.Parser (Parser)
import qualified Parser.Parser as Parser


topLevel :: Parser ()
topLevel = do
    position <- Parser.getPosition

    if Position.column position == 1 then
        return ()

    else
        Parser.fail <| Error.TopLevelIndentationExpected position


sameLine :: Parser ()
sameLine = do
    referencePosition <- Parser.getReferencePosition
    position <- Parser.getPosition

    let isOnSameLine = Position.line position == Position.line referencePosition

    if isOnSameLine then
        return ()

    else
        Parser.fail <| Error.SameLineExpected position


sameLineOrIndented :: Parser ()
sameLineOrIndented = do
    referencePosition <- Parser.getReferencePosition
    position <- Parser.getPosition

    let isOnSameLine = Position.line position == Position.line referencePosition
    let relativeIndentation =
            Position.column position - Position.column referencePosition

    if isOnSameLine || relativeIndentation > 0 then
        return ()

    else
        Parser.fail <| Error.SameLineOrIndentedExpected position


withPositionReference :: Parser a -> Parser a
withPositionReference parser = do
    initialReference <- Parser.getReferencePosition
    currentPosition <- Parser.getPosition

    Parser.setReferencePosition currentPosition
    result <- parser

    Parser.setReferencePosition initialReference
    return result
