module Check.Type.ReplaceTopLevel
    ( Definition(..)
    , Expression(..)
    , replace
    ) where

import qualified Data.Map as Map

import qualified AST.Expression as E
import AST.Identifier (DataId(..), ReferenceId)
import qualified AST.Identifier as Identifier
import AST.TypeAnnotation (TypeAnnotation)
import Check.Type.Context (Context)
import qualified Check.Type.Context as Context
import qualified Check.Type.Model.PrimitiveType as Primitive
import Parser.Model.Quote (Quote)
import Utils.NonEmpty (NonEmpty)
import qualified Utils.Maybe as Maybe
import qualified Utils.Tuple as Tuple


data Expression
    = Primitive Quote Primitive.Type
    | Reference ReferenceId
    | ContextReference TypeAnnotation
    | LetIn
        { definitions :: NonEmpty Definition
        , body :: Expression
        }
        deriving (Eq, Show)


data Definition
    = SimpleDefinition DataId Expression
        deriving (Eq, Show)


replace :: Context -> [ (DataId, TypeAnnotation) ] -> E.Expression -> Expression
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
        E.Value value ->
            replaceValue value


        E.Reference referenceId ->
            let
                referenceType =
                    Context.lookupReference
                        (Identifier.formatReferenceId referenceId)
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


replaceValue :: E.Value -> Expression
replaceValue value =
    case value of
        E.Bool (E.TrueLiteral quote) ->
            Primitive quote Primitive.Bool

        E.Bool (E.FalseLiteral quote) ->
            Primitive quote Primitive.Bool

        E.Int quote _ ->
            Primitive quote Primitive.Int

        E.Float quote _ ->
            Primitive quote Primitive.Float

        E.Char quote _ ->
            Primitive quote Primitive.Char

        E.String quote _ ->
            Primitive quote Primitive.String
