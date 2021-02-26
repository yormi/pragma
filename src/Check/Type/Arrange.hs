module Check.Type.Arrange
    ( Expression(..)
    , Link(..)
    , Value(..)
    , arrange
    ) where

import qualified Control.Monad.State as State
import qualified Control.Monad.Trans.Writer as Writer

import qualified Check.Type.Futurize as F
import Parser.Model.Quote (Quote)
import qualified Check.Type.Cycle as Cycle
import Check.Type.Model (Type)
import qualified Utils.List as List
import qualified Utils.NonEmpty as NonEmpty
import qualified Utils.Set as Set


newtype Link =
    Link Int
        deriving (Eq, Show)


data Expression
    = Value
        { link :: Link
        , value :: Value
        }
    | ContextReference
        { link :: Link
        , type_ :: Type
        }
    | Future
        { link :: Link
        , future :: F.Placeholder
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
    = Int Quote Int
    | Float Quote Float
        deriving (Eq, Show)


type Arranger a =
    Writer.WriterT [Expression] (State.State NextLink) a


type NextLink =
    Link


nextLink :: Arranger Link
nextLink = do
    next@(Link n) <- lift <| State.get
    lift <| State.put <| Link (n + 1)
    return next


arrangeNext :: Expression -> Arranger ()
arrangeNext =
    List.singleton >> Writer.tell


-- ACTION


arrange :: F.Expression -> [Expression]
arrange futurized =
    let
        initialState =
            Link 0
    in
    arranger futurized
        |> Writer.execWriterT
        |> flip State.evalState initialState


arranger :: F.Expression -> Arranger Link
arranger futurized =
    case futurized of
        F.Value (F.Int quote n) -> do
            link <- nextLink
            Int quote n
                |> Value link
                |> arrangeNext
            return link

        F.ContextReference type_ -> do
            link <- nextLink
            ContextReference link type_
                |> arrangeNext
            return link

        F.Future placeholder -> do
            link <- nextLink
            Future link placeholder
                |> arrangeNext
            return link

        F.LetIn { definitions, body } ->
            let
                letFutures =
                    [] -- TODO

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

                graph =
                    definitions
                        |> NonEmpty.toList
                        |> map
                            (\F.Definition
                                { placeholder=future, body=definitionBody } ->
                                ( future
                                , dependancesOnCurrentLetDefinitions
                                    definitionBody
                                    |> Set.filter (\f -> f /= future)
                                )
                            )
                        |> Cycle.defineGraph
            in do
            case Cycle.dependencySort graph of
                Right dependencySorted ->
                    dependencySorted
                        |> traverse
                            (\F.Definition
                                { placeholder=future, body=definitionBody }
                                -> do
                                bodyLink <- arranger definitionBody
                                link <- nextLink
                                Definition link future bodyLink
                                    |> arrangeNext
                            )
                        |> void

                Left _ ->
                    fail

            arranger body
