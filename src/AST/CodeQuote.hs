module AST.CodeQuote
    ( CodeQuote(..)
    , Position(..)
    , fromPositions
    , toPosition
    ) where


data CodeQuote =
    CodeQuote
        { filename :: String
        , fromLine :: Int
        , fromColumn :: Int
        , toLine :: Int
        , toColumn :: Int
        }
        deriving (Eq, Show)


data Position =
    Position
        { filename :: String
        , line :: Int
        , column :: Int
        }
        deriving (Eq, Show)


fromPositions :: Position -> Position -> CodeQuote
fromPositions from to =
    CodeQuote
        { filename = filename (from :: Position)
        , fromLine = line from
        , fromColumn = column from
        , toLine = line to
        , toColumn = column to
        }


toPosition :: CodeQuote -> Position
toPosition q =
    Position (filename (q :: CodeQuote)) (fromLine q) (fromColumn q)
