module Type.Constraint.Gatherer.Gather (gather) where

import qualified Data.List as List

import qualified AST.Module as M
import qualified AST.TypeAnnotation as TypeAnnotation
import Type.Constraint.Gatherer.Model (Gatherer)
import qualified Type.Constraint.Gatherer.Model as Gatherer
import Type.Constraint.Model (Constraint)
import qualified Type.Constraint.Model as Constraint
import qualified Type.Constraint.Gatherer.Module as Module
import qualified Type as T
import qualified Utils.Maybe as Maybe


type ModuleContext =
    [(M.FunctionName, T.Type)]


gather :: M.Module -> M.TopLevel -> Either Gatherer.ConstraintError [Constraint]
gather (M.Module topLevels) topLevel =
    Gatherer.gatherConstraints <| do
        context <- moduleContext topLevels
        gatherTopLevel context topLevel


gatherTopLevel :: ModuleContext -> M.TopLevel -> Gatherer ()
gatherTopLevel context topLevel = do
    Module.gather topLevel
        |> Gatherer.withDataReferences context


moduleContext :: [M.TopLevel] -> Gatherer ModuleContext
moduleContext topLevels =
    topLevels
        |> map
            ( \t ->
                case t of
                    M.Function { M.functionName, M.typeAnnotation } ->
                        Just (functionName, typeAnnotation)

                    _ ->
                        Nothing
            )
        |> Maybe.values
        |> traverse
            (\(functionName, typeAnnotation) ->
                let
                    hasTypeVariable =
                        TypeAnnotation.extractTypeVariables
                            typeAnnotation
                            |> \ts -> List.length ts > 0

                in do
                signatureType <-
                    Module.signatureType typeAnnotation

                if hasTypeVariable then do
                    returnType <- Gatherer.freshVariable
                    Constraint.Generalized signatureType returnType
                        |> Gatherer.addConstraint

                    (functionName, T.Variable returnType)
                        |> return
                else
                    (functionName, signatureType)
                        |> return
            )

