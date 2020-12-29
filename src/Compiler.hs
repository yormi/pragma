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
import qualified Utils.Either as Either
import qualified Utils.List as List


type Compiler a =
    ExceptT [CompilerError] IO a


data CompilerError
    = ParsingError Parser.ParserError
    | ContextError Context.Error
    | ConstraintGatheringError Gatherer.ConstraintError
    | ConstraintSolvingError ConstraintSolver.SolvingError
    deriving (Eq, Show)


run :: Compiler a -> IO (Either [CompilerError] a)
run =
    Except.runExceptT


fromEither :: Either CompilerError a -> Compiler a
fromEither =
    Either.fold (List.singleton >> Except.throwError) return


fromEithers :: [Either CompilerError a] -> Compiler [a]
fromEithers subSteps =
    case Either.lefts subSteps of
        [] ->
            return <| Either.rights subSteps

        lefts ->
            Except.throwError lefts


liftIO :: IO a -> Compiler a
liftIO =
    Except.liftIO
