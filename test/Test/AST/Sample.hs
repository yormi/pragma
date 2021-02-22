module Test.AST.Sample
    ( aConstructor
    , aCustomType
    ) where

import qualified Test.Parser.Sample as ParserSample

import qualified AST.Module as M
import qualified AST.Identifier as Identifier
import Parser.Model.Position (Position(..))
import qualified Utils.NonEmpty as NonEmpty
import qualified Utils.OrderedSet as OrderedSet


aCustomType :: String -> M.TopLevel
aCustomType typeName =
    M.SumType
        { fromPosition = ParserSample.aPosition
        , typeName = Identifier.TypeId ParserSample.aQuote typeName
        , typeVariables = OrderedSet.empty
        , dataChoices = NonEmpty.singleton (aConstructor "aConstructor")
        }


aConstructor :: String -> M.DataChoice
aConstructor constructorName =
    M.DataChoice
        (Identifier.ConstructorId ParserSample.aQuote constructorName)
        []

