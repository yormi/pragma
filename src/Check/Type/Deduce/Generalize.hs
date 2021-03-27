module Check.Type.Deduce.Generalize
    ( generalize
    ) where

import Check.Type.Deduce.Deducer (Deducer)
import Check.Type.Deduce.Model.GenericType (GenericType)
import qualified Check.Type.Deduce.Model.GenericType as G
import qualified Check.Type.Model.Type as T


-- TDD the shit outta this
-- Generalize unboundTypes !?!?
--
-- Do I really need the Deducer here ?
--          --> NOPE, onw Generalizer to store next variable
--
-- Add the variables in the definition --> NOPE, info dupplication
--
generalize :: T.Type -> Deducer GenericType
generalize type_ =
    case type_ of
        T.Bool ->
            return G.Bool

        T.Int ->
            return G.Int

        T.Float ->
            return G.Float

        T.Char ->
            return G.Char

        T.String ->
            return G.String

        T.Function { arg, returning } ->
            return G.Bool

        T.Unbound { name, instanceType } ->
            return G.Bool

        T.Custom { name, args } ->
            return G.Bool
--     let
--         placeholderList =
--             Set.toList expressionPlaceholders
--     in do
--     precised <- Deduce.mostPrecised type_
--     variables <- traverse (const Solver.nextVariable) placeholderList
--     let variableMapping =
--             List.zip placeholderList variables
--                 |> Map.fromList
-- 
--     replacePlaceholders variableMapping precised
-- 
-- 
-- replacePlaceholders
--     :: Map T.TypePlaceholder TypeVariableId
--     -> InstancedType
--     -> G.GenericType
-- replacePlaceholders variableMapping type_ =
--     case type_ of
--         Bool ->
--             G.Bool
-- 
--         Int ->
--             G.Int
-- 
--         Float ->
--             G.Float
-- 
--         Char ->
--             G.Char
-- 
--         String ->
--             G.String
-- 
--         Function arg returnType ->
--             let
--                 argAnnotation =
--                     replacePlaceholders variableMapping arg
--                 returnAnnotation =
--                     replacePlaceholders variableMapping returnType
--             in
--             G.Function argAnnotation returnAnnotation
-- 
--         Custom typeName args -> do
--             args
--                 |> map (replacePlaceholders variableMapping)
--                 |> G.Custom typeName
-- 
--         Unbound name placeholder ->
--             G.ParentVariable name placeholder
-- 
--         Placeholder placeholder ->
--             let
--                 replacement =
--                     Map.lookup placeholder variableMapping
--             in
--             case replacement of
--                 Just v ->
--                     G.Variable v
-- 
--                 Nothing ->
--                     G.Placeholder placeholder
