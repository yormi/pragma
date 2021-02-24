module Check.Type.Arrange
    ( Expression(..)
    , LinkPlaceholder(..)
    , Value(..)
    , arrange
    ) where

import qualified Control.Monad.State as State
import qualified Control.Monad.Trans.Writer as Writer

import qualified Check.Type.Futurize as F
import Parser.Model.Quote (Quote)
import Check.Type.Model (Type)
import qualified Utils.List as List
import qualified Utils.NonEmpty as NonEmpty


newtype LinkPlaceholder =
    LinkPlaceholder Int
        deriving (Eq, Show)


data Expression
    = Value
        { link :: LinkPlaceholder
        , value :: Value
        }
    | ContextReference
        { link :: LinkPlaceholder
        , type_ :: Type
        }
    | Future
        { link :: LinkPlaceholder
        , future :: F.Placeholder
        }
    | Definition
        { link :: LinkPlaceholder
        , futurePlaceholder :: F.Placeholder
        , body :: LinkPlaceholder
        }
    | OrderedIf
        { condition :: LinkPlaceholder
        , whenTrue :: LinkPlaceholder
        , whenFalse :: LinkPlaceholder
        , returns :: LinkPlaceholder
        }
        deriving (Eq, Show)


data Value
    = Int Quote Int
    | Float Quote Float
        deriving (Eq, Show)


type Arranger a =
    Writer.WriterT [Expression] (State.State NextLink) a


type NextLink =
    LinkPlaceholder


nextLink :: Arranger LinkPlaceholder
nextLink = do
    next@(LinkPlaceholder n) <- lift <| State.get
    lift <| State.put <| LinkPlaceholder (n + 1)
    return next


arrangeNext :: Expression -> Arranger ()
arrangeNext =
    List.singleton >> Writer.tell


-- ACTION


arrange :: F.Expression -> [Expression]
arrange futurized =
    let
        initialState =
            LinkPlaceholder 0
    in
    arranger futurized
        |> Writer.execWriterT
        |> flip State.evalState initialState


arranger :: F.Expression -> Arranger LinkPlaceholder
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

        F.LetIn { definitions, body } -> do
            definitions
                |> NonEmpty.toList
                |> traverse
                    (\F.Definition { placeholder=future, body=definitionBody } -> do
                        bodyLink <- arranger definitionBody
                        link <- nextLink
                        Definition link future bodyLink
                            |> arrangeNext
                    )
                |> void
            arranger body
