module Type.Constraint.Solver.Generic
    ( GenericType(..)
    , extractTypeVariables
    , fromTypeAnnotation
    ) where

import Control.Monad.Writer (Writer)
import qualified Control.Monad.Writer as Writer
import qualified Data.Set as Set

import AST.Identifier (TypeId, TypeVariableId)
import AST.TypeAnnotation (TypeAnnotation)
import qualified AST.TypeAnnotation as TA
import Type.Constraint.Solver.Instanced (InstanceId)
import qualified Type.Model as T


data GenericType
    = Bool
    | Int
    | Float
    | Char
    | String
    | Function
        { arg :: GenericType
        , returnType :: GenericType
        }
    | Custom
        { typeName :: TypeId
        , args :: [GenericType]
        }
    | Instance InstanceId
    | Variable TypeVariableId
    | Placeholder T.TypePlaceholder
    deriving (Eq, Show)


fromTypeAnnotation :: TypeAnnotation -> GenericType
fromTypeAnnotation annotation =
    case annotation of
        TA.Bool ->
            Bool

        TA.Int ->
            Int

        TA.Float ->
            Float

        TA.Char ->
            Char

        TA.String ->
            String

        TA.Function { arg, returnType } ->
            Function (fromTypeAnnotation arg) (fromTypeAnnotation returnType)

        TA.Custom { typeName, args } ->
            Custom typeName (map fromTypeAnnotation args)

        TA.Variable id ->
            Variable id


extractTypeVariables :: GenericType -> Set TypeVariableId
extractTypeVariables genericType =
    let
        collectTypeVariable :: GenericType -> Writer [TypeVariableId] ()
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
