module Check.Type.ReplaceTopLevel
    ( Definition(..)
    , Expression(..)
    , Value(..)
    , replace
    ) where

import qualified Data.Map as Map

import qualified AST.Expression as E
import AST.Identifier (DataId(..), ReferenceId)
import qualified AST.Identifier as Identifier
import Check.Type.Context (Context)
import Check.Type.Model (Type)
import Parser.Model.Quote (Quote)
import Utils.NonEmpty (NonEmpty)
import qualified Utils.Maybe as Maybe
import qualified Utils.Tuple as Tuple


data Expression
    = Value Value
    | Reference ReferenceId
    | ContextReference Type
    | LetIn
        { definitions :: NonEmpty Definition
        , body :: Expression
        }
        deriving (Eq, Show)


data Definition
    = SimpleDefinition DataId Expression
        deriving (Eq, Show)


data Value
    = Int Quote Int
    | Float Quote Float
        deriving (Eq, Show)


replace :: Context -> [ (DataId, Type) ] -> E.Expression -> Expression
replace moduleContext params expression =
    let
        contextWithParams =
            params
                |> map (Tuple.mapFirst (\(DataId _ id) -> id))
                |> Map.fromList
                |> Map.union moduleContext
    in
    replaceWithContext contextWithParams expression


replaceWithContext :: Context -> E.Expression -> Expression
replaceWithContext context expression =
    case expression of
        E.Value (E.Int quote n) ->
            Value <| Int quote n


        E.Reference referenceId ->
            let
                referenceType =
                    Map.lookup (Identifier.formatReferenceId referenceId)
                        context
            in
            referenceType
                |> map ContextReference
                |> Maybe.withDefault (Reference referenceId)


        E.LetIn { definitions, body } ->
            let
                replacedDefinitions =
                    map
                        (\(E.SimpleDefinition dataId definition) ->
                            replaceWithContext context definition
                                |> SimpleDefinition dataId
                        )
                        definitions

                replacedBody =
                    replaceWithContext context body
            in
            LetIn
                { definitions = replacedDefinitions
                , body = replacedBody
                }
