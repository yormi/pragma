module Type.Constraint.Solver.Generic
    ( instantiate
    , instantiateFunctionParams
    ) where

import qualified Data.Map as Map
import qualified Data.Set as Set

import AST.Identifier (TypeVariableId)
import Type.Constraint.Solver.Model (Solver)
import qualified Type.Constraint.Solver.Model as Solver
import qualified Type.Model as T
import qualified Utils.List as List


instantiate :: T.Type -> Solver T.Type
instantiate generalizedType = do
    replacements <- instancePlaceholders generalizedType
    T.replaceVariables replacements generalizedType
        |> return


instantiateFunctionParams :: T.Type -> T.Type -> Solver (T.Type, T.Type)
instantiateFunctionParams signatureType definitionType = do
    replacements <- instancePlaceholders signatureType
    let signature = T.replaceVariables replacements signatureType
    let definition = T.replaceVariables replacements definitionType
    return (signature, definition)


instancePlaceholders :: T.Type -> Solver (Map TypeVariableId T.Type)
instancePlaceholders type_ =
    let
        variables =
            T.variables type_
                |> Set.toList
    in do
    instances <- traverse (const Solver.nextPlaceholder) variables
    instances
        |> List.zip variables
        |> Map.fromList
        |> return
