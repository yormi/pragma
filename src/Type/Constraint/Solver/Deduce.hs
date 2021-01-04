module Type.Constraint.Solver.Deduce
    ( mostPrecised
    ) where

import qualified Data.Map as Map

import Type.Constraint.Solver.Model.Solver (InstancedType(..), Solver)
import qualified Type.Constraint.Solver.Instantiate as Instantiate
import qualified Type.Constraint.Solver.Model.Solver as Solver
import qualified Type.Constraint.Solver.Model.Solution as Solution


mostPrecised :: InstancedType -> Solver InstancedType
mostPrecised type_ =
    case type_ of
        Placeholder p -> do
            solution <- Solver.deducedSoFar
            let morePrecise = Map.lookup p solution

            case morePrecise of
                Just (Solution.Generic _ genericType) -> do
                    Instantiate.fromGenericType genericType

                Just (Solution.Instanced instancedType) ->
                    mostPrecised instancedType

                _ ->
                    return type_


        Custom typeId args -> do
            precisedArgs <-
                traverse mostPrecised args
            Custom typeId precisedArgs
                |> return


        Function arg returnType -> do
            precisedArg <- mostPrecised arg
            precisedReturnType <- mostPrecised returnType
            Function precisedArg precisedReturnType
                |> return


        _ ->
            return type_
