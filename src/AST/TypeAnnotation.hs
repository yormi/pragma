module AST.TypeAnnotation
    ( Identifier
    , TypeAnnotation(..)
    , extractTypeVariables
    ) where

import Control.Monad.Writer (Writer)
import qualified Control.Monad.Writer as Writer
import qualified Utils.List as List


data TypeAnnotation
    = Bool
    | Int
    | Float
    | Char
    | String
    | Function
        { arg :: TypeAnnotation
        , returnType :: TypeAnnotation
        }
    | Variable String
    deriving (Eq, Show)


type Identifier = String


extractTypeVariables :: TypeAnnotation -> [Identifier]
extractTypeVariables typeAnnotation =
    let
        collectTypeVariable :: TypeAnnotation -> Writer [Identifier] ()
        collectTypeVariable annotation =
            case annotation of
                Variable identifier ->
                    Writer.tell [identifier]

                Function { arg , returnType } -> do
                    collectTypeVariable arg
                    collectTypeVariable returnType

                _ ->
                    return ()
    in
    collectTypeVariable typeAnnotation
        |> Writer.execWriter
        |> List.unique
