module Test.Parser.Sample
    ( aFilePath
    , aPosition
    , aQuote
    ) where

import Parser.Model.Position (Position(..))
import Parser.Model.Quote (Quote(..))


aFilePath :: FilePath
aFilePath =
    "some/File/Path.pa"


aPosition :: Position
aPosition =
    Position aFilePath 1 1


aQuote :: Quote
aQuote =
    Quote aFilePath 1 1 1 1

