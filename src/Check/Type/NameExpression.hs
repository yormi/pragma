module Check.Type.NameExpression
    ( Error(..)
    , Expression(..)
    , Link(..)
    , arrange
    ) where

import qualified Control.Monad.State as State
import Control.Monad.Trans.Except (ExceptT)
import qualified Control.Monad.Trans.Except as Except
import qualified Control.Monad.Trans.Writer as Writer

import AST.TypeAnnotation (TypeAnnotation)
import qualified Check.Type.Futurize as F
import qualified Check.Type.Model.PrimitiveType as Primitive
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


data Definition =
    Definition
        { link :: Link
        , bodyLink :: Link
        , expression :: [ Expression ]
        }


data Expression
    = Primitive
        { link :: Link
        , quote :: Quote
        , primitiveType :: Primitive.Type
        }
    | ContextReference
        { link :: Link
        , annotation :: TypeAnnotation
        }
    | Future
        { link :: Link
        }
    | If
        { condition :: Link
        , whenTrue :: Link
        , whenFalse :: Link
        , returns :: Link
        }
        deriving (Eq, Show)


newtype Error
    = ThisIsABug String
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


registerPlaceholder :: F.Placeholder -> Link -> Arranger ()
registerPlaceholder placeholder link =
    State.modify
        (\state ->
            placeholderLinks state
                |> Map.insert placeholder link
                |> \ls -> state { placeholderLinks = ls }
        )


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

arranger :: F.Expression -> Arranger ()
arranger e =
    return ()


namedExpression :: F.Expression -> ( [ Definition ], [ Expression ] )
namedExpression expression =
    case expression of
        -- F.LetIn { definitions, letInBody } -> do
        --     (defs, expressions) <-
        --         definitions
        --             |> map (F.definitionBody  >> namedExpression)
        --             |> fold () []
        --     namedExpression letInBody


        _ ->
            ([], [])
