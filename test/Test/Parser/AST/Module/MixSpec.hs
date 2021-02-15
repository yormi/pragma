module Test.Parser.AST.Module.MixSpec where

import Test.Hspec hiding (context)

import qualified Test.Utils as TestUtils
import qualified Test.Parser.Utils as Utils

import qualified AST.Module as Module
import qualified AST.TypeAnnotation as Annotation
import qualified Parser.AST.Module as Module
import qualified Utils.List as List
import qualified Utils.NonEmpty as NonEmpty
import qualified Utils.OrderedSet as OrderedSet
import qualified Utils.Either as Either
import qualified Utils.String as String


spec :: Spec
spec =
    let
        run sourceCode =
            Utils.run sourceCode Module.moduleParser
    in
    describe "Module Parser Mix" <| do
        it "Parses correctly a function after a sumType" <|
            let
                source =
                    [ "type Color a"
                    , "    = Red a"
                    , ""
                    , ""
                    , "foo : Int -> Int"
                    , "foo x ="
                    , "    x"
                    ]
                        |> String.mergeLines
            in
            run source `shouldSatisfy` Either.isRight
