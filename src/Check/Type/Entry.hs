module Check.Type.Entry
    ( typeCheck
    ) where

import qualified Control.Monad as Monad

import AST.Module (Module(..))
import Check.Type.Check (TypeError)
import qualified Check.Type.Arrange as Arrange
import qualified Check.Type.Check as Check
import Check.Type.Constraint (Constraint)
import qualified Check.Type.Constraint as Constraint
import Check.Type.Context (Context)
import qualified Check.Type.Context as Context
import Check.Type.Deduce (Deductions)
import qualified Check.Type.Deduce as Deduce
import qualified Check.Type.Futurize as Futurize
import qualified Check.Type.ReplaceTopLevel as ReplaceTopLevel
import Check.Type.TopLevelData (TopLevelData(..))
import qualified Check.Type.TopLevelData as TopLevelData
import qualified Utils.Either as Either


data Error
    = FuturizeError Futurize.Error
    | ArrangeError Arrange.Error
    | DeduceError Deduce.Error
    | ConstraintError Constraint.Error
    | TypeCheckErrors [TypeError]


typeCheck :: Module -> [Error]
typeCheck module_@(Module topLevels) =
    let
        moduleContext =
            Context.build module_
    in
    topLevels
        |> TopLevelData.filter moduleContext
        |> map
            (\topLevel -> do
                arranged <- arrange moduleContext topLevel
                deduced <- deduce arranged
                constraints <- buildConstraint deduced arranged
                check constraints
            )
        |> Either.lefts


arrange :: Context -> TopLevelData -> Either Error [ Arrange.Expression ]
arrange moduleContext TopLevelData { paramTypes, body } =
    ReplaceTopLevel.replace moduleContext paramTypes body
        |> Futurize.futurize
        |> Either.mapLeft FuturizeError
        |> bind (Arrange.arrange >> Either.mapLeft ArrangeError)


deduce :: [ Arrange.Expression ] -> Either Error Deductions
deduce =
    Deduce.deduceType >> Either.mapLeft DeduceError


buildConstraint
    :: Deductions -> [ Arrange.Expression ] -> Either Error [ Constraint ]
buildConstraint deduced =
    Constraint.build deduced
        >> Either.mapLeft ConstraintError


check :: [ Constraint ] -> Either Error ()
check deduced =
    Monad.mapM Check.check deduced
        |> map TypeCheckErrors
        |> Either.fromMaybeError
