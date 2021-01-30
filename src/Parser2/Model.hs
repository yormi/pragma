module Parser2.Model
    ( Column
    , Parsed(..)
    , Position(..)
    , Quote(..)
    , SourceCode
    , fromPosition
    , positionToColumn
    , quoteCode
    )
    where

import qualified GHC.Show


data Parsed a =
    Parsed
        { quote :: Quote
        , result :: a
        }
        deriving (Eq, Show)


data Quote =
    Quote
        { filePath :: String
        , fromLine :: Int
        , fromColumn :: Int
        , toLine :: Int
        , toColumn :: Int
        }
        deriving (Eq)


instance Show Quote where
    show q =
        show (fromLine q) ++ ":" ++ show (fromColumn q)
            ++ "-"
            ++ show (toLine q) ++ ":" ++ show (toColumn q)


quoteCode :: Position -> Position -> Quote
quoteCode from to =
    Quote
        { filePath = filePath (from :: Position)
        , fromLine = line from
        , fromColumn = column from
        , toLine = line to
        , toColumn = column to
        }


fromPosition :: Quote -> Position
fromPosition q =
    Position
        { filePath = filePath (q :: Quote)
        , line = fromLine q
        , column = fromColumn q
        }


type SourceCode
    = String


newtype Column
    = Column Int
    deriving (Eq, Show)


positionToColumn :: Position -> Column
positionToColumn =
    Column << column


data Position
    = Position
        { filePath :: String
        , line :: Int
        , column :: Int
        }
        deriving (Eq)


instance Show Position where
    show (Position _ line column) =
        show line ++ ":" ++ show column
