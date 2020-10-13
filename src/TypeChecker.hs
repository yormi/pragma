module TypeChecker
    ( check
    , showCheckResult
    ) where


import Control.Monad.Trans.Class (lift)
import Control.Monad.Reader (ReaderT, runReaderT)
import qualified Data.List as List

import qualified Expression as E
import qualified Type as T
import qualified Module as Module
import Module (Module)
import qualified Utils.Either as Either


type Check = ReaderT Env (Either TypeError) T.Type


type Env = [(Name, T.Type)]


type Name = String


data TypeError
    = Mismatch T.Type T.Type
    | NotFunction T.Type
    | NotInScope Name
    deriving (Eq, Show)


showCheckResult :: [Check] -> String
showCheckResult =
    map (flip runReaderT [])
        >> map (Either.fold (\e -> "FAIL - " ++ show e) show)
        >> List.intercalate "\n"


check :: Module -> [Check]
check (Module.Module topLevels) =
    topLevels
        |> map (\t ->
            case t of
                Module.Function { Module.body } -> do
                    type_ <- checkExpr body
                    if type_ == T.Int then
                        return T.Int

                    else
                        lift <| Left <| Mismatch T.Int type_
        )


checkExpr :: E.Expr -> Check
checkExpr expr =
    case expr of
        E.Value v ->
            checkValue v

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
