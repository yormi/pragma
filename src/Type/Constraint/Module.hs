module Type.Constraint.Module (gather) where

import qualified Data.List as List

import qualified AST.Module as M
import qualified Type.Constraint.Expression as Expression
import Type.Constraint.Gatherer (Gatherer)
import qualified Type.Constraint.Gatherer as Gatherer
import qualified Type.Constraint.Model as Constraint


gather :: M.TopLevel -> Gatherer ()
gather topLevel =
    case topLevel of
        M.Function { M.type_, M.params, M.body } -> do
            paramTypes <- traverse (const Gatherer.freshVariable) params
            let paramWithTypes = List.zip params paramTypes
            bodyType <-
                Gatherer.withEnv paramWithTypes <| Expression.gather body

            Constraint.Function
                { Constraint.functionType = type_
                , Constraint.params = paramTypes
                , Constraint.body = bodyType
                }
                |> Gatherer.addConstraint
