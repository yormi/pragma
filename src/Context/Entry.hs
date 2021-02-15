module Context.Entry
    ( moduleContext
    ) where

import qualified Data.Map as Map

import AST.TypeAnnotation (TypeAnnotation)
import AST.Module (TopLevel)
import qualified Context.Constructor as Constructor
import qualified Context.Data as Data
import Parser.Model.Quote (Quote)
import qualified Utils.List as List
import qualified Utils.Maybe as Maybe


-- Pass ModuleContext to the following steps:
--   * Gather - For ReferencesInScope
--   * Solve - For inital constraints


type ModuleContext =
    Map Reference TypeAnnotation


type Reference =
    String


data Error
    = ConstructorNameClash Reference [Quote]
    | DataNameClash Reference [Quote]
    deriving (Eq, Show)


moduleContext :: [TopLevel] -> Either [Error] ModuleContext
moduleContext topLevels =
    let
        constructors =
            Constructor.context topLevels

        data_ =
            Data.context topLevels

    in
    case (constructorClashes constructors, dataClashes data_) of
        ([], []) ->
            let
                constructorReferences =
                    map (\c -> (Constructor.name c, Constructor.annotation c))
                        constructors

                dataReferences =
                    map (\c -> (Data.name c, Data.annotation c)) data_
            in
            constructorReferences ++ dataReferences
                |> Map.fromList
                |> Right

        _ ->
            constructorClashes constructors ++ dataClashes data_
                |> Left


constructorClashes :: [Constructor.Declaration] -> [Error]
constructorClashes =
    List.groupBy (\a b -> Constructor.name a == Constructor.name b)
        >> List.filter (\g -> List.length g > 1)
        >> map
            (\declarations ->
                let
                    name =
                        declarations
                            |> map Constructor.name
                            |> List.head
                            |> Maybe.withDefault "Won't happen"

                    occurences =
                        map Constructor.quote declarations
                in
                ConstructorNameClash name occurences
            )


dataClashes :: [Data.Declaration] -> [Error]
dataClashes =
    List.groupBy (\a b -> Data.name a == Data.name b)
        >> List.filter (\g -> List.length g > 1)
        >> map
            (\declarations ->
                let
                    name =
                        declarations
                            |> map Data.name
                            |> List.head
                            |> Maybe.withDefault "Won't happen"

                    occurences =
                        map Data.quote declarations
                in
                DataNameClash name occurences
            )
