module TypeChecker
    ( check
    , showCheckResult
    ) where


import Control.Monad.Trans.Class (lift)
import Control.Monad.Reader (ReaderT, runReaderT)
import qualified Control.Monad.Reader as Reader
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Tuple as Tuple

import qualified Expression as E
import qualified Type as T
import qualified Module as M
import Module (Module)
import qualified Utils.Either as Either


-- TODO Change type... currently an error won't cumulate errors
type Check = ReaderT Env (Either TypeError) T.Type


type Env = [(E.Identifier, T.Type)]


data TypeError
    = Mismatch
        { expected :: T.Type
        , actual :: T.Type
        }
    | FunctionBodyMismatch
        { env :: Env
        , returningType :: T.Type
        , bodyType :: T.Type
        }
    | TooManyParam
        { functionType :: T.Type
        , params :: [E.Identifier]
        }
    | NotFunction T.Type
    | NotInScope E.Identifier
    | IfConditionMustBeBool E.Expr
    | BothIfExpressionsMustHaveSameType E.Expr E.Expr
    deriving (Eq, Show)


failCheck :: TypeError -> Check
failCheck =
    Left >> lift


-- TODO priorize according to latest scope
withEnv :: [( E.Identifier, T.Type )] -> Check -> Check
withEnv reference =
    Reader.local (\env -> reference ++ env)


lookupReference :: E.Identifier -> Check
lookupReference id = do
  env <- Reader.ask
  case List.lookup id env of
    Just value  ->
        return value

    Nothing ->
        failCheck <| NotInScope id


--


showCheckResult :: [Check] -> String
showCheckResult =
    map (flip runReaderT [])
        >> map (Either.fold (\e -> "FAIL - " ++ show e) show)
        >> List.intercalate "\n"


check :: Module -> [Check]
check (M.Module topLevels) =
    topLevels
        |> map (\t ->
            case t of
                M.Function { M.type_, M.params, M.body } -> do
                    case type_ of
                        Just t -> do
                            functionType <- returningFunctionType t params

                            currentEnv <- Reader.ask

                            let ps = paramsWithTypes t params
                            expressionType  <-
                                withEnv ps <| checkExpr body

                            if functionType == expressionType then
                                return functionType

                            else
                                FunctionBodyMismatch
                                    (ps ++ currentEnv)
                                    functionType expressionType
                                    |> failCheck

                        Nothing ->
                            checkExpr body
        )


-- TODO Shadowing
-- TODO Merge with returningFunctionType
paramsWithTypes :: T.Type -> [E.Identifier] -> [( E.Identifier, T.Type )]
paramsWithTypes functionType functionParams =
    functionParams
        |> List.foldl
            (\(currentType, params) paramId -> do
                case currentType of
                    T.Function a b ->
                        (paramId, a) : params
                            |> (\newEnv -> (b, newEnv))

                    _ ->
                        (currentType, [])
            )
            (functionType, [])
        |> Tuple.snd


returningFunctionType :: T.Type -> [E.Identifier] -> Check
returningFunctionType type_ params =
    List.foldl
        (\result _ -> do
            currentType <- result

            case currentType of
                T.Function _ returnValue ->
                    return returnValue

                _ ->
                    failCheck <| TooManyParam type_ params
        )
        (return type_)
        params


checkExpr :: E.Expr -> Check
checkExpr expr =
    case expr of
        E.Value v ->
            checkValue v

        E.Reference r ->
            lookupReference r

        E.If { E.condition , E.whenTrue , E.whenFalse } -> do
            conditionType <- checkExpr condition
            whenTrueType <- checkExpr whenTrue
            whenFalseType <- checkExpr whenFalse

            if conditionType /= T.Bool then
                failCheck <| IfConditionMustBeBool condition

            else if whenTrueType /= whenFalseType then
                BothIfExpressionsMustHaveSameType whenTrue whenFalse
                    |> failCheck

            else
                return whenTrueType

        _ ->
            return T.Bool


checkValue :: E.Value -> Check
checkValue v =
    case v of
        E.Bool b ->
            return T.Bool

        E.Char c ->
            return T.Char

        E.Float d ->
            return T.Float

        E.Int i ->
            return T.Int

        E.String str ->
            return T.String
