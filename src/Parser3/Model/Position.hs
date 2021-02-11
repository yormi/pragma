module Parser3.Model.Position
    ( Column
    , Position(..)
    , toColumn
    )
    where

import qualified GHC.Show


newtype Column
    = Column Int
    deriving (Eq, Show)


data Position
    = Position
        { filePath :: FilePath
        , line :: Int
        , column :: Int
        }
        deriving (Eq)


instance Show Position where
    show (Position _ line column) =
        show line ++ ":" ++ show column


toColumn :: Position -> Column
toColumn =
    Column << column
