module Type.Constraint.Solver.Instanced
    ( InstanceId(..)
    , InstancedType(..)
    , fromType
    ) where

import AST.Identifier (TypeId)
import qualified Type.Model as T


data InstancedType
    = Bool
    | Int
    | Float
    | Char
    | String
    | Function
        { arg :: InstancedType
        , returnType :: InstancedType
        }
    | Custom
        { typeId :: TypeId
        , args :: [InstancedType]
        }
    | Instance InstanceId
    | Placeholder T.TypePlaceholder
    deriving (Eq, Show)


data InstanceId
    = InstanceId Int
    deriving (Eq, Show)


fromType :: T.Type -> InstancedType
fromType type_ =
    case type_ of
        T.Bool ->
            Bool

        T.Int ->
            Int

        T.Float ->
            Float

        T.Char ->
            Char

        T.String ->
            String

        T.Function (T.FunctionType arg returnType) ->
            let
                argAnnotation =
                    fromType arg

                returnAnnotation =
                    fromType returnType
            in
            Function argAnnotation returnAnnotation

        T.Custom typeName args ->
            map fromType args
                |> Custom typeName

        T.Placeholder p ->
            Placeholder p
