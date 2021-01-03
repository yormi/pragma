module AST.TypeAnnotation
    ( TypeAnnotation(..)
    , extractTypeVariables
    ) where

import Control.Monad.Writer (Writer)
import qualified Control.Monad.Writer as Writer
import qualified Data.Set as Set

import AST.Identifier (TypeId, TypeVariableId)


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
    | Custom
        { typeName :: TypeId
        , args :: [TypeAnnotation]
        }
    | Variable TypeVariableId
    deriving (Eq, Show)


extractTypeVariables :: TypeAnnotation -> Set TypeVariableId
extractTypeVariables genericType =
    let
        collectTypeVariable :: TypeAnnotation -> Writer [TypeVariableId] ()
        collectTypeVariable annotation =
            case annotation of
                Variable identifier ->
                    Writer.tell [identifier]

                Function { arg , returnType } -> do
                    collectTypeVariable arg
                    collectTypeVariable returnType

                Custom { args } ->
                    traverse collectTypeVariable args
                        |> void

                _ ->
                    return ()
    in
    collectTypeVariable genericType
        |> Writer.execWriter
        |> Set.fromList
