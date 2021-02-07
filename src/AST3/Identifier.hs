module AST3.Identifier
    ( ConstructorId
    , DataId
    , ReferenceId
    , TypeId
    , TypeVariableId

    , constructorId
    , dataId
    , referenceId
    , typeId
    , typeVariableId

    , dataIdForTest
    , typeIdForTest
    , typeVariableIdForTest

    , generateTypeVariableId

    , dataOrConstructor
    , formatConstructorId
    , formatDataId
    , formatReferenceId
    , formatTypeId
    , formatTypeVariableId
    )
    where

import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Char as Char

import Parser3.Quote (Quote)
import qualified Utils.List as List


newtype ConstructorId
    = ConstructorId String
    deriving (Eq, Ord, Show)


newtype DataId
    = DataId String
    deriving (Eq, Generic, Ord, Show, FromJSON, ToJSON)


data ReferenceId
    = ReferenceId Quote String
    deriving (Eq, Show)


newtype TypeId
    = TypeId String
    deriving (Eq, Ord, Show)


newtype TypeVariableId
    = TypeVariableId String
    deriving (Eq, Ord, Show)


constructorId :: String -> Maybe ConstructorId
constructorId str =
    str
        |> List.head
        |> bind
            (\firstChar ->
                if Char.isUpper firstChar then
                    Just <| ConstructorId str

                else
                    Nothing
            )



dataId :: String -> Maybe DataId
dataId str =
    str
        |> List.head
        |> bind
            (\firstChar ->
                if Char.isLower firstChar then
                    Just <| DataId str

                else
                    Nothing
            )


referenceId :: Quote -> String -> ReferenceId
referenceId =
    ReferenceId


typeId :: String -> Maybe TypeId
typeId str =
    str
        |> List.head
        |> bind
            (\firstChar ->
                if Char.isUpper firstChar then
                    Just <| TypeId str

                else
                    Nothing
            )


typeVariableId :: String -> Maybe TypeVariableId
typeVariableId str =
    str
        |> List.head
        |> bind
            (\firstChar ->
                if Char.isLower firstChar then
                    Just <| TypeVariableId str

                else
                    Nothing
            )


dataIdForTest :: String -> DataId
dataIdForTest =
    DataId


typeIdForTest :: String -> TypeId
typeIdForTest =
    TypeId


typeVariableIdForTest :: String -> TypeVariableId
typeVariableIdForTest =
    TypeVariableId


generateTypeVariableId :: Int -> TypeVariableId
generateTypeVariableId n =
    TypeVariableId <| "a" ++ show n


--- FORMAT ---


formatConstructorId :: ConstructorId -> String
formatConstructorId (ConstructorId str) =
    str

formatDataId :: DataId -> String
formatDataId (DataId str) =
    str


formatReferenceId :: ReferenceId -> String
formatReferenceId (ReferenceId _ str) =
    str


formatTypeId :: TypeId -> String
formatTypeId (TypeId str) =
    str


formatTypeVariableId :: TypeVariableId -> String
formatTypeVariableId (TypeVariableId str) =
    str


--- SPECIALIZE ---

dataOrConstructor :: ReferenceId -> Either ConstructorId DataId
dataOrConstructor (ReferenceId _ str) =
    let
        firstChar =
            List.head str
    in
    case firstChar of
        Just c ->
            if Char.isUpper c then
                Left <| ConstructorId str

            else
                Right <| DataId str

        Nothing ->
            Left <| ConstructorId str -- Swallow this case since it should not happen
