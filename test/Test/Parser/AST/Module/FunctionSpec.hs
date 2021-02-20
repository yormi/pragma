module Test.Parser.AST.Module.FunctionSpec where

import Test.Hspec hiding (context)

import qualified Test.Utils as TestUtils
import qualified Test.Parser.Utils as Utils

import qualified AST.Identifier as Identifier
import qualified AST.Module as Module
import qualified AST.TypeAnnotation as Annotation
import qualified Parser.AST.Module as Module
import qualified Utils.List as List
import qualified Utils.String as String


spec :: Spec
spec =
    let
        run sourceCode =
            Utils.run sourceCode Module.moduleParser
    in
    describe "Module Parser - Function" <| do
        it "Parses correctly a function with the returning type being a Sum Type" <|
            let
                source =
                    [ "foo : Color a"
                    , "foo ="
                    , "    3"
                    ]
                        |> String.mergeLines
            in do
            functionName <-
                Identifier.dataId (Utils.quote 2 1 2 3) "foo"
                    |> TestUtils.assumeJust
            colorId <-
                Identifier.typeId (Utils.quote 1 7 1 11) "Color"
                    |> TestUtils.assumeJust
            typeVariableId <-
                Identifier.typeVariableId (Utils.quote 1 13 1 13) "a"
                    |> TestUtils.assumeJust
            let expected =
                    Module.Function
                        { quote = Utils.quote 1 1 3 5
                        , typeAnnotation =
                            Annotation.Custom
                                colorId
                                [Annotation.Variable typeVariableId]
                        , functionName = functionName
                        , params = []
                        , body = Utils.int 3 5 3
                        }
                        |> List.singleton
                        |> Module.Module
                        |> Right
            run source `shouldBe` expected
