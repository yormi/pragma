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


gather :: M.Module -> M.TopLevel -> Either Gatherer.ConstraintError [Constraint]
gather module_ topLevel =
    gatherModule module_ topLevel
        |> Gatherer.gatherConstraints


gatherModule :: M.Module -> M.TopLevel -> Gatherer ()
gatherModule (M.Module topLevels) topLevel = do
    context <-
        topLevels
            |> map
                (\t ->
                    case t of
                        M.Function { M.functionName, M.typeAnnotation } ->
                            Just <|
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

                        _ ->
                            Nothing

                )
            |> Maybe.keepValues
            |> sequence

    Module.gather topLevel
        |> Gatherer.withDataReferences context
