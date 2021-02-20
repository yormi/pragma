module AST.Expression
    ( BoolLiteral(..)
    , Case(..)
    , Definition(..)
    , Expression(..)
    , Pattern(..)
    , Value(..)
    , quote
    ) where


import Data.List.NonEmpty (NonEmpty)

import Parser.Model.Position (Position)
import Parser.Model.Quote (Quote(..))
import qualified Parser.Model.Quote as Quote
import AST.Identifier (DataId, ReferenceId)
import qualified AST.Identifier as Identifier
import qualified Utils.NonEmpty as NonEmpty


data Expression
    = Value Value
    | Reference ReferenceId
    | If
        { fromPosition :: Position
        , condition :: Expression
        , whenTrue :: Expression
        , whenFalse :: Expression
        }
    | LetIn
        { fromPosition :: Position
        , definitions :: NonEmpty Definition
        , body :: Expression
        }
    | CaseOf
        { fromPosition :: Position
        , element :: Expression
        , cases :: NonEmpty Case
        }
    | Lambda
        { fromPosition :: Position
        , params :: NonEmpty DataId
        , body :: Expression
        }
    | Application
        { functionName :: ReferenceId
        , args :: NonEmpty Expression
        }
    deriving (Eq, Show)


data Definition
    = SimpleDefinition DataId Expression
    deriving (Eq, Show)


data Case
    = Case
        { pattern_ :: Pattern
        , caseBody :: Expression
        }
    deriving (Eq, Show)


data Pattern
    = WildCardPattern
    | ValuePattern Value
    | IdentifierPattern DataId
    | TuplePattern Pattern Pattern
    deriving (Eq, Show)


data Value
    = Bool BoolLiteral
    | Char Quote Char
    | Float Quote Double
    | Int Quote Int
    | String Quote String
    deriving (Eq, Show)


data BoolLiteral
    = TrueLiteral Quote
    | FalseLiteral Quote
    deriving (Eq, Show)


quote :: Expression -> Quote
quote expression =
    case expression of
        Value value ->
            valueQuote value

        Reference referenceId ->
            Identifier.referenceQuote referenceId

        If { fromPosition, whenFalse } ->
            quote whenFalse
                |> Quote.to
                |> Quote.fromPositions fromPosition

        LetIn { fromPosition, body } ->
            quote body
                |> Quote.to
                |> Quote.fromPositions fromPosition

        CaseOf { fromPosition, cases } ->
            cases
                |> NonEmpty.last
                |> caseBody
                |> quote
                |> Quote.to
                |> Quote.fromPositions fromPosition

        Lambda { fromPosition, body } ->
            quote body
                |> Quote.to
                |> Quote.fromPositions fromPosition

        Application { functionName, args } ->
            let
                from =
                    Identifier.referenceQuote functionName
                        |> Quote.from

                to =
                    args
                        |> NonEmpty.last
                        |> quote
                        |> Quote.to
            in
            Quote.fromPositions from to


valueQuote :: Value -> Quote
valueQuote value =
    case value of
        Bool (FalseLiteral q) ->
            q

        Bool (TrueLiteral q) ->
            q

        Char q _ ->
            q

        Float q _ ->
            q

        Int q _ ->
            q

        String q _ ->
            q
