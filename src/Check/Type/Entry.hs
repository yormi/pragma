module Check.Type.Entry
    ( typeCheck
    ) where

import AST.Module (Module(..))
import qualified Check.Type.Arrange as Arrange
import qualified Check.Type.Check as Check
import qualified Check.Type.Check.Model.Error as Check
import qualified Check.Type.Constraint as Constraint
import Check.Type.Context (Context)
import qualified Check.Type.Context as Context
import qualified Check.Type.Futurize as Futurize
import qualified Check.Type.ReplaceTopLevel as ReplaceTopLevel
import Check.Type.TopLevelData (TopLevelData(..))
import qualified Check.Type.TopLevelData as TopLevelData
import qualified Utils.Either as Either
import qualified Utils.Maybe as Maybe


data Error
    = FuturizeError Futurize.Error
    | ArrangeError Arrange.Error
    | CheckErrors Check.Error


typeCheck :: Module -> [Error]
typeCheck module_@(Module topLevels) =
    let
        moduleContext =
            Context.build module_
    in
    topLevels
        |> TopLevelData.filter moduleContext
        |> map (typeCheckTopLevelData moduleContext)
        |> Maybe.values


typeCheckTopLevelData :: Context -> TopLevelData -> Maybe Error
typeCheckTopLevelData moduleContext topLevel = do
    arrange moduleContext topLevel
        |> bind
            ( Constraint.build
                >> Check.check
                >> map CheckErrors
                >> Either.fromMaybeError
            )
        |> Either.toMaybeError


arrange :: Context -> TopLevelData -> Either Error [ Arrange.Expression ]
arrange moduleContext TopLevelData { paramTypes, body } =
    ReplaceTopLevel.replace moduleContext paramTypes body
        |> Futurize.futurize
        |> Either.mapLeft FuturizeError
        |> bind (Arrange.arrange >> Either.mapLeft ArrangeError)
