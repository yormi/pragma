module Check.Type.Arrange
    ( Error(..)
    , Expression(..)
    , Link(..)
    , Value(..)
    , arrange
    ) where

import qualified Control.Monad.State as State
import Control.Monad.Trans.Except (ExceptT)
import qualified Control.Monad.Trans.Except as Except
import qualified Control.Monad.Trans.Writer as Writer

import AST.TypeAnnotation (TypeAnnotation)
import qualified Check.Type.Futurize as F
import Parser.Model.Quote (Quote)
import qualified Check.Type.Cycle as Cycle
import qualified Utils.Either as Either
import qualified Utils.List as List
import qualified Utils.Maybe as Maybe
import qualified Utils.Map as Map
import Utils.NonEmpty (NonEmpty)
import qualified Utils.NonEmpty as NonEmpty
import qualified Utils.Set as Set


newtype Link =
    Link Int
        deriving (Eq, Ord, Show)


data Expression
    = Value
        { link :: Link
        , value :: Value
        }
    | ContextReference
        { link :: Link
        , annotation :: TypeAnnotation
        }
    | Future
        { link :: Link
        }
    | Definition
        { link :: Link
        , futurePlaceholder :: F.Placeholder
        , body :: Link
        }
    | OrderedIf
        { condition :: Link
        , whenTrue :: Link
        , whenFalse :: Link
        , returns :: Link
        }
        deriving (Eq, Show)


data Value
    = Int Quote
    | Float Quote
        deriving (Eq, Show)


data Error
    = CycleInLetDefinitionDependencies
    | ThisIsABug String
        deriving (Eq, Show)


type Arranger a =
    ExceptT Error
        (Writer.WriterT [Expression]
            (State.State State)
        )
        a


data State =
    State
        { next :: Link
        , placeholderLinks :: Map F.Placeholder Link
        }


nextLink :: Arranger Link
nextLink = do
    nextOne <- State.gets next
    State.modify
        (\state ->
            let
                (Link n) =
                    next state
            in
            state { next = Link (n + 1) }
        )
    return nextOne


placeholderLink :: F.Placeholder -> Arranger Link
placeholderLink placeholder = do
    links <- State.gets placeholderLinks
    case Map.lookup placeholder links of
        Just link ->
            return link

        Nothing ->
            ThisIsABug "The placeholder must have already been registered with its Link"
                |> fail


arrangeNext :: Expression -> Arranger ()
arrangeNext =
    List.singleton >> Writer.tell >> lift


fail :: Error -> Arranger a
fail =
    Except.throwE


-- ACTION


arrange :: F.Expression -> Either Error [Expression]
arrange futurized =
    let
        initialState =
            State
                { next = Link 0
                , placeholderLinks = Map.empty
                }
    in
    arranger futurized
        |> Except.runExceptT
        |> Writer.runWriterT
        |> flip State.evalState initialState
        |> \(result, arranged) -> map (const arranged) result


arranger :: F.Expression -> Arranger Link
arranger futurized =
    case futurized of
        F.Value (F.Int quote) -> do
            link <- nextLink
            Int quote
                |> Value link
                |> arrangeNext
            return link

        F.ContextReference type_ -> do
            link <- nextLink
            ContextReference link type_
                |> arrangeNext
            return link

        F.Future placeholder -> do
            link <- placeholderLink placeholder
            arrangeNext <| Future link
            return link

        F.LetIn { definitions, body } -> do
            letDefinitionsArranger definitions
            arranger body


letDefinitionsArranger :: NonEmpty F.Definition -> Arranger ()
letDefinitionsArranger definitions=
    let
        placeholderToDefinition def =
            let
                isMatchingDefinition definition =
                    F.placeholder definition == def
            in
            List.find isMatchingDefinition definitions
    in
    case dependencySortedDefinitions definitions of
        Right sortedDependencies ->
            let
                sorted =
                    map placeholderToDefinition sortedDependencies
            in
            if List.all Maybe.isJust sorted then
                sorted
                    |> Maybe.values
                    |> traverse definitionArranger
                    |> void
            else
                ThisIsABug
                    "The placeholder must have a matching definition"
                    |> fail

        Left e ->
            fail e


dependencySortedDefinitions
    :: NonEmpty F.Definition -> Either Error [ F.Placeholder ]
dependencySortedDefinitions definitions =
    let
        letFutures =
            definitions
                |> NonEmpty.toList
                |> map F.placeholder
                |> Set.fromList

        dependancesOnCurrentLetDefinitions futurized =
            case futurized of
                F.Value _ ->
                    Set.empty

                F.ContextReference _ ->
                    Set.empty

                F.Future future ->
                    if Set.contains future letFutures then
                        Set.singleton future

                    else
                        Set.empty

                F.LetIn {} ->
                    Set.empty
                    -- TODO - dependenceOnCurrentLetDefinitions

        toGraphNode definition =
            ( F.placeholder definition
            , dependancesOnCurrentLetDefinitions
                (F.body (definition :: F.Definition))
                |> Set.filter (\f -> f /= F.placeholder definition)
            )

        graph =
            definitions
                |> NonEmpty.toList
                |> map toGraphNode
                |> Cycle.defineGraph
    in
    Cycle.dependencySort graph
        |> Either.mapLeft (const CycleInLetDefinitionDependencies)


definitionArranger :: F.Definition -> Arranger ()
definitionArranger F.Definition { placeholder=future, body=definitionBody } = do
    bodyLink <- arranger definitionBody
    link <- nextLink
    Definition link future bodyLink
        |> arrangeNext
