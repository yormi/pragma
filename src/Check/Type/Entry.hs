module Check.Type.Entry
    ( typeCheck
    ) where

import qualified Control.Monad as Monad

import AST.Module (Module(..))
import Check.Type.Check (TypeError)
import qualified Check.Type.ArrangeResolution as Arrange
import qualified Check.Type.Check as Check
import qualified Check.Type.DeduceType as Deduce
import qualified Check.Type.ReplaceReference as Replace
import qualified Check.Type.Context as Context
import qualified Utils.Maybe as Maybe


typeCheck :: Module -> [TypeError]
typeCheck module_@(Module topLevels) =
    let
        moduleContext =
            Context.build module_
    in
    topLevels
        |> map
            (Replace.replaceReference moduleContext
                >> Arrange.arrange
                >> Deduce.deduceType
                >> Monad.mapM Check.check
            )
        |> Maybe.values
        |> join
