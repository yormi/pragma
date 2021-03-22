module Check.Type.Constraint
    ( Constraint
    , Error
    , build
    ) where


import qualified Check.Type.Arrange as A
import qualified Check.Type.Deduce.Entry as D
import Parser.Model.Quote (Quote)
import Check.Type.Model.Type (Type)


data Constraint
    = If
        { condition :: (Quote, Type)
        , whenTrue :: (Quote, Type)
        , whenFalse :: (Quote, Type)
        }
    deriving (Eq, Show)


newtype Error
    = ThisIsABug String
    deriving (Eq, Show)


-- ACTION


build :: D.Deductions -> [A.Expression] -> Either Error [Constraint]
build _ _ =
    Right []
