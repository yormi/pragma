module Type.Constraint.Module (gather) where

import qualified Data.List as List

import qualified AST.Expression as E
import qualified AST.Module as M
import qualified Type as T
import qualified Type.Constraint.Expression as Expression
import Type.Constraint.Gatherer (Gatherer)
import qualified Type.Constraint.Gatherer as Gatherer


gather :: M.TopLevel -> Gatherer T.Type
gather topLevel =
    case topLevel of
        M.Function { M.type_, M.params, M.body } -> do
            functionType <- gatherFunction params body
            Gatherer.addConstraint type_ functionType
            return functionType



gatherFunction :: [E.Identifier] -> E.Expr -> Gatherer T.Type
gatherFunction params body = do
    paramTypes <- traverse (const Gatherer.freshVariable) params
    let paramWithTypes = List.zip params paramTypes
    bodyType_ <- Gatherer.withEnv paramWithTypes <| Expression.gather body

    return <|
        List.foldl
            (\functionType paramType ->
                T.FunctionType (T.Variable paramType) functionType
                    |> T.Function
            )
            bodyType_
            paramTypes
