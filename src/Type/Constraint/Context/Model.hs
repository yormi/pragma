module Type.Constraint.Context.Model
    ( Context(..)
    , Error(..)
    , addData
    , context
    , lookupReference
    ) where

import AST.Identifier (DataId, ReferenceId)
import qualified AST.Identifier as Identifier
import qualified AST.Module as M
import qualified Type.Model as T
import qualified Type.Constraint.Context.Data as Data
import qualified Type.Constraint.Context.Type as Type
import qualified Type.Constraint.Context.Constructor as Constructor
import qualified Utils.Either as Either


data Error
    = TypeError Type.Error
    | ConstructorError
    | DataError Data.Error
    deriving (Eq, Show)


data Context
    = Context
        { type_ :: Type.Context
        , constructor :: Constructor.Context
        , data_ :: Data.Context
        }
        deriving (Eq, Show)


context :: [M.TopLevel] -> Either Error Context
context topLevels = do
    typeContext <-
        Type.context topLevels
            |> Either.mapLeft TypeError

    let constructorContext = Constructor.context typeContext topLevels

    dataContext <-
        Data.context topLevels
            |> Either.mapLeft DataError

    Context typeContext constructorContext dataContext
        |> return


addData :: DataId -> T.Type -> Context -> Either Error Context
addData dataId type_ mainContext =
    let
        dataContext =
            data_ mainContext
    in
        Data.addData dataId type_ dataContext
            |> map (\d -> mainContext { data_ = d })
            |> Either.mapLeft DataError


lookupReference :: ReferenceId -> Context -> Maybe T.Type
lookupReference identifier mainContext =
    let
        specializedId =
            Identifier.dataOrConstructor identifier
    in
    case specializedId of
        Right dataId ->
            Data.lookup dataId (data_ mainContext)

        Left constructorId ->
            Constructor.lookup constructorId (constructor mainContext)
