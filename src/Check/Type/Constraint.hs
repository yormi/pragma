module Check.Type.Constraint
    ( Constraint
    , Error
    , build
    ) where


import qualified Check.Type.Arrange as A
import qualified Check.Type.Deduce as D
import Parser.Model.Quote (Quote)
import Check.Type.Model (Type)


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


-- Handle Instanciation
build :: D.Deductions -> [A.Expression] -> Either Error [Constraint]
build deductions arrangedExpression =
    Right []
