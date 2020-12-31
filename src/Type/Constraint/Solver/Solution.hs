module Type.Constraint.Solver.Solution
    ( mostPrecised
    ) where

import qualified Data.Map as Map

import Type.Constraint.Solver.Model (Solver)
import qualified Type.Constraint.Solver.Model as Solver
import Type.Constraint.Solver.Generic (instantiate)
import qualified Type.Model as T


mostPrecised :: T.Type -> Solver T.Type
mostPrecised type_ =
    case type_ of
        T.Placeholder p -> do
            solution <- Solver.deducedSoFar
            let morePrecise = Map.lookup p solution

            case morePrecise of
                Just (Solver.ReferenceType _ referenceType) -> do
                    precised <- mostPrecised referenceType
                    instantiate precised

                Just (Solver.InstanceType instancedType) ->
                    mostPrecised instancedType

                _ ->
                    return type_


        T.Custom typeId args -> do
            precisedArgs <-
                traverse mostPrecised args
            T.Custom typeId precisedArgs
                |> return


        T.Function (T.FunctionType arg returnType) -> do
            precisedArg <- mostPrecised arg
            precisedReturnType <- mostPrecised returnType
            T.FunctionType precisedArg precisedReturnType
                |> T.Function
                |> return


        _ ->
            return type_


