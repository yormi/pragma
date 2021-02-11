module Parser3.Indentation
    ( topLevel
    , sameLineOrIndented
    , withPositionReference
    )
    where


import qualified Parser3.Combinator as C
import Parser3.Model.Error (Error(..))
import qualified Parser3.Model.Position as Position
import Parser3.Parser (Parser)
import qualified Parser3.Parser as Parser


topLevel :: Parser ()
topLevel = do
    C.someSpace
    position <- Parser.getPosition

    if Position.column position == 1 then
        return ()

    else
        Parser.fail <| TopLevelIndentationExpected position


sameLineOrIndented :: Parser ()
sameLineOrIndented = do
    C.someSpace
    referencePosition <- Parser.getReferencePosition
    position <- Parser.getPosition

    let isOnSameLine = Position.line position == Position.line referencePosition
    let relativeIndentation =
            Position.column position - Position.column referencePosition

    if isOnSameLine || relativeIndentation > 0 then
        return ()

    else
        Parser.fail <| SameLineOrIndentedExpected position


withPositionReference :: Parser a -> Parser a
withPositionReference parser = do
    C.someSpace
    initialReference <- Parser.getReferencePosition
    currentPosition <- Parser.getPosition

    Parser.setReferencePosition currentPosition
    result <- parser

    Parser.setReferencePosition initialReference
    return result
