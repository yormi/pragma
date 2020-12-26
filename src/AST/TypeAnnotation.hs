module AST.TypeAnnotation
    ( TypeAnnotation(..)
    , extractTypeVariables
    ) where

import Control.Monad.Writer (Writer)
import qualified Control.Monad.Writer as Writer

import AST.Identifier (TypeId, TypeVariableId)
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
    | Custom TypeId
    | Variable TypeVariableId
    deriving (Eq, Show)


extractTypeVariables :: TypeAnnotation -> [TypeVariableId]
extractTypeVariables typeAnnotation =
    let
        collectTypeVariable :: TypeAnnotation -> Writer [TypeVariableId] ()
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
