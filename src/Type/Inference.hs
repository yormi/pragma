module Type.Inference () where

import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.State (StateT)
import qualified Control.Monad.State as State
import Control.Monad.Reader (ReaderT)
import qualified Control.Monad.Reader as Reader

import qualified AST.Expression as E
import qualified AST.Module as M
import qualified Type as T


--type TypeCheck = Program -> Either TypeError SolvedType


type GatherConstraints = Program -> Either ConstraintError [Constraint]


--type SolveType = [Constraint] -> Either SolvingError SolvedType


data SolvedType
    = Bool
    | Char
    | Int
    | Float
    | String
    | Function
        { input :: SolvedType
        , output :: SolvedType
        }
    deriving (Eq, Show)


type TypeSolution = Map TypeVariable SolvedType


type TypeVariable = Int


data Program = Program
    { env :: [M.TopLevel]
    , main :: E.Expr
    }




-- Constraint

type Constraint = (T.Type, T.Type)

data TypeScheme
    = TypeScheme
        { variables :: [TypeVariable]
        , type_ :: T.Type
        }


type ConstraintGatherer a =
    ReaderT
        TypeEnv
        (StateT
            NextTypeVariable
            (Either ConstraintError)
        )
        a


type TypeEnv = Map TypeName TypeVariable


type TypeName = String


type NextTypeVariable = TypeVariable


data ConstraintError = None


data Result =
    Result
        { constraints :: [Constraint]
        , resultType :: T.Type
        }


--infer :: TypeCheck
--infer =
--    ()


gatherConstraints :: GatherConstraints
gatherConstraints Program { env, main } =
    --map gatherTopLevel env
    -- checkMain
    Right []


gatherTopLevel :: M.TopLevel -> ConstraintGatherer Result
gatherTopLevel topLevel =
    case topLevel of
        M.Function { M.type_, M.functionName, M.params, M.body } -> do
            functionResult <- gatherFunction params body
            let constraint = (type_, resultType functionResult)
            Result
                (constraint : constraints functionResult)
                (resultType functionResult)
                |> return


gatherFunction ::
    [E.Identifier] -> E.Expr -> ConstraintGatherer Result
gatherFunction params body = do
    paramTypes <- traverse (const freshVariable) params
    let paramWithTypes = List.zip params paramTypes
    bodyResult <- withEnv paramWithTypes <| gatherExpression

    let functionType =
            List.foldl
                (\functionType paramType ->
                    T.FunctionType (T.Variable paramType) functionType
                        |> T.Function
                )
                (resultType bodyResult)
                paramTypes

    return <| Result (constraints bodyResult) functionType 


gatherExpression :: ConstraintGatherer Result
gatherExpression =
    return <| Result [] T.Bool


withEnv ::
    [(TypeName, TypeVariable)] -> ConstraintGatherer a -> ConstraintGatherer a
withEnv newEnv = do
  let scope oldEnv =
        List.foldl
            (\env (name, typeVariable) ->
                env
                    |> removeFromEnv name
                    |> extendEnv (name, typeVariable)
            )
            oldEnv
            newEnv
  Reader.local scope


removeFromEnv :: TypeName -> TypeEnv -> TypeEnv
removeFromEnv typeVariable env =
    Map.delete typeVariable env


extendEnv :: (TypeName, TypeVariable) -> TypeEnv -> TypeEnv
extendEnv (name, typeVariable) env =
    Map.insert name typeVariable env


freshVariable :: ConstraintGatherer TypeVariable
freshVariable = do
    nextTypeVariable <- State.get
    State.put <| nextTypeVariable + 1
    return nextTypeVariable
