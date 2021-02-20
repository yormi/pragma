module Context.Model
    ( Context(..)
    , Error(..)
    , addLetDefinition
    , context
    , lookupReference
    ) where

import AST.Identifier (DataId, ReferenceId)
import qualified AST.Identifier as Identifier
import qualified AST.Module as M
import qualified Type.Model as T
import qualified Context.Data as Data
import qualified Context.Type as Type
import qualified Context.Constructor as Constructor
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

    let constructorContext = Constructor.context topLevels

    dataContext <-
        Data.context topLevels
            |> Either.mapLeft DataError

    Context typeContext constructorContext dataContext
        |> return


addLetDefinition
    :: DataId -> T.TypePlaceholder -> Context -> Either Error Context
addLetDefinition dataId placeholder mainContext =
    let
        dataContext =
            data_ mainContext
    in
        Data.addLetDefinition dataId placeholder dataContext
            |> map (\d -> mainContext { data_ = d })
            |> Either.mapLeft DataError


lookupReference :: ReferenceId -> Context -> Maybe Data.DataTypeInfo
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
                |> map Data.TopLevel
