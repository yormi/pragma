module Type.Constraint.Model (Constraint(..)) where

import Data.List.NonEmpty (NonEmpty)

import qualified Type as T


data Constraint
    = Simple
        { newComing :: T.Type
        , concludedFrom :: T.Type
        }
    | IfThenElse
        { condition :: T.Type
        , whenTrue :: T.Type
        , whenFalse :: T.Type
        , returnType :: T.Type
        }
    | Application
        { functionReference :: T.Type
        , args :: NonEmpty T.Type
        , returnType :: T.Type
        }
    | Function
        { functionType :: T.Type
        , params :: [T.Type]
        , body :: T.Type
        }
    deriving (Eq, Show)
