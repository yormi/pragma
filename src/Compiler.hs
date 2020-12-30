module Compiler
    ( Compiler
    , CompilerError(..)
    , fromEither
    , fromEithers
    , liftIO
    , run
    ) where

import Control.Monad.Except (ExceptT)
import qualified Control.Monad.Except as Except

import qualified Parser.Parser as Parser
import qualified Type.Constraint.Context.Model as Context
import qualified Type.Constraint.Gatherer.Model as Gatherer
import qualified Type.Constraint.Solver.Solve as ConstraintSolver
import qualified Type.ValidateAnnotation as ValidateAnnotation
import qualified Utils.Either as Either
import qualified Utils.List as List


type Compiler a =
    ExceptT [CompilerError] IO a


data CompilerError
    = TypeValidationError ValidateAnnotation.Error
    | ParsingError Parser.ParserError
    | ContextError Context.Error
    | ConstraintGatheringError Gatherer.ConstraintError
    | ConstraintSolvingError ConstraintSolver.SolvingError
    deriving (Eq, Show)


run :: Compiler a -> IO (Either [CompilerError] a)
run =
    Except.runExceptT


fail :: [CompilerError] -> Compiler a
fail =
    Except.throwError


fromEither :: Either CompilerError a -> Compiler a
fromEither =
    Either.fold (List.singleton >> fail) return


fromEithers :: Show a => [Either CompilerError a] -> Compiler [a]
fromEithers subSteps =
    let
        lefts =
            Either.lefts subSteps
    in
    if List.isEmpty lefts then
        Either.rights subSteps
            |> return

    else
        fail lefts


liftIO :: IO a -> Compiler a
liftIO =
    Except.liftIO
