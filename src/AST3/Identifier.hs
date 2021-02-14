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

    , typeVariableQuote

    , dataOrConstructor
    , formatConstructorId
    , formatDataId
    , formatReferenceId
    , formatTypeId
    , formatTypeVariableId
    )
    where

import qualified Data.Char as Char

import Parser3.Model.Quote (Quote)
import qualified Utils.List as List


data ConstructorId
    = ConstructorId Quote String
    deriving (Eq, Ord, Show)


data DataId
    = DataId Quote String
    deriving (Eq, Show)


data ReferenceId
    = ReferenceId Quote String
    deriving (Eq, Show)


data TypeId
    = TypeId Quote String
    deriving (Eq, Show)


data TypeVariableId
    = TypeVariableId Quote String
    deriving (Eq, Ord, Show)


constructorId :: Quote -> String -> Maybe ConstructorId
constructorId quote str =
    str
        |> List.head
        |> bind
            (\firstChar ->
                if Char.isUpper firstChar then
                    Just <| ConstructorId quote str

                else
                    Nothing
            )



dataId :: Quote -> String -> Maybe DataId
dataId quote str =
    str
        |> List.head
        |> bind
            (\firstChar ->
                if Char.isLower firstChar then
                    Just <| DataId quote str

                else
                    Nothing
            )


referenceId :: Quote -> String -> ReferenceId
referenceId =
    ReferenceId


typeId :: Quote -> String -> Maybe TypeId
typeId quote str =
    str
        |> List.head
        |> bind
            (\firstChar ->
                if Char.isUpper firstChar then
                    Just <| TypeId quote str

                else
                    Nothing
            )


typeVariableId :: Quote -> String -> Maybe TypeVariableId
typeVariableId quote str =
    str
        |> List.head
        |> bind
            (\firstChar ->
                if Char.isLower firstChar then
                    Just <| TypeVariableId quote str

                else
                    Nothing
            )


typeVariableQuote :: TypeVariableId -> Quote
typeVariableQuote (TypeVariableId quote _) =
    quote



--- FORMAT ---


formatConstructorId :: ConstructorId -> String
formatConstructorId (ConstructorId _ str) =
    str


formatDataId :: DataId -> String
formatDataId (DataId _ str) =
    str


formatReferenceId :: ReferenceId -> String
formatReferenceId (ReferenceId _ str) =
    str


formatTypeId :: TypeId -> String
formatTypeId (TypeId _ str) =
    str


formatTypeVariableId :: TypeVariableId -> String
formatTypeVariableId (TypeVariableId _ str) =
    str


--- SPECIALIZE ---

dataOrConstructor :: ReferenceId -> Either ConstructorId DataId
dataOrConstructor (ReferenceId quote str) =
    let
        firstChar =
            List.head str
    in
    case firstChar of
        Just c ->
            if Char.isUpper c then
                Left <| ConstructorId quote str

            else
                Right <| DataId quote str

        Nothing ->
            -- Swallow this case since the parser should have errored
            -- on empty string
            Left <| ConstructorId quote str
