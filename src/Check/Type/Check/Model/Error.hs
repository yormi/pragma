module Check.Type.Check.Model.Error (Error(..)) where

import AST.Identifier (ReferenceId)
import AST.TypeAnnotation (TypeAnnotation)
import Check.Type.Check.Model.Solution
    ( Solution
    , SolutionType
    )
import Check.Type.Check.Model.InstancedType
    ( InstanceId
    , InstancedType(..)
    , UnboundId
    )


data Error
    = TypeVariableCannotSatisfyBothConstraint
        SolutionType
        SolutionType
        Solution
    | IfConditionMustBeABool
        { type_ :: SolutionType
        }
    | BothIfAlternativesMustHaveSameType
        { whenTrue :: InstancedType
        , whenFalse :: InstancedType
        }
    | BadApplication
        { functionName :: ReferenceId
        , referenceType :: InstancedType
        , functionType :: InstancedType
        }
    | FunctionDefinitionMustMatchType
        { signatureType :: TypeAnnotation
        , definitionType :: InstancedType
        }
    | ThisIsABug String
    deriving (Eq, Show)
