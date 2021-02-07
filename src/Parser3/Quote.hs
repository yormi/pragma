module Parser3.Quote
    ( Quote(..)
    , from
    , fromPositions
    , to
    )
    where

import qualified GHC.Show

import Parser3.Position (Position(..))
import qualified Parser3.Position as Position


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


fromPositions :: Position -> Position -> Quote
fromPositions fromPosition toPosition =
    Quote
        { filePath = Position.filePath fromPosition
        , fromLine = Position.line fromPosition
        , fromColumn = Position.column fromPosition
        , toLine = Position.line toPosition
        , toColumn = Position.column toPosition
        }


from :: Quote -> Position
from q =
    Position
        { filePath = filePath (q :: Quote)
        , line = fromLine q
        , column = fromColumn q
        }


to :: Quote -> Position
to q =
    Position
        { filePath = filePath (q :: Quote)
        , line = toLine q
        , column = toColumn q
        }
