module Test.Check.Type.ContextSpec where

import Test.Hspec hiding (context)

import qualified Data.Map as Map

import qualified Test.AST.Sample as Sample
import qualified Test.Parser.Sample as ParserSample

import qualified AST.Expression as E
import qualified AST.Identifier as Identifier
import qualified AST.Module as M
import qualified AST.TypeAnnotation as TA
import qualified Check.Type.Context as Context
import qualified Utils.NonEmpty as NonEmpty
import qualified Utils.OrderedSet as OrderedSet


spec :: Spec
spec =
    let
        aFunction =
            M.Function
                { quote = ParserSample.aQuote
                , typeAnnotation = TA.Int
                , functionName =
                    Identifier.DataId ParserSample.aQuote "functionName"
                , params = []
                , body = E.Value <| E.Int ParserSample.aQuote 3
                }

        aCustomType =
            (Sample.aCustomType "customTypeName")
                { M.typeVariables =
                    Identifier.TypeVariableId ParserSample.aQuote "a"
                        |> OrderedSet.singleton
                , M.dataChoices =
                    NonEmpty.build
                        (Sample.aConstructor "ctor1")
                        [ Sample.aConstructor "ctor2" ]
                }
    in
    describe "Context" <| do
        it "adds one data definition to the context" <|
            let
                module_ =
                    M.Module [ aFunction ]

                actual =
                    Context.build module_

                expected =
                    Map.fromList [("functionName", TA.Int)]
            in
            actual `shouldBe` expected


        it "adds many data definitions to the context" <|
            let
                module_ =
                    [ aFunction
                        { M.typeAnnotation = TA.Int
                        , M.functionName =
                            Identifier.DataId ParserSample.aQuote "function1"
                        }
                    , aFunction
                        { M.typeAnnotation = TA.String
                        , M.functionName =
                            Identifier.DataId ParserSample.aQuote "function2"
                        }
                    ]
                        |> M.Module

                actual =
                    Context.build module_

                expected =
                    Map.fromList
                        [ ("function1", TA.Int)
                        , ("function2", TA.String)
                        ]
            in
            actual `shouldBe` expected



        it "adds the constructors of a custom type to the context" <|
            let
                module_ =
                    M.Module [ aCustomType ]

                actual =
                    Context.build module_

                expectedConstructor constructorName =
                    ( constructorName
                    , TA.Custom (M.typeName aCustomType) []
                    )

                expected =
                    Map.fromList
                        [ expectedConstructor "ctor1"
                        , expectedConstructor "ctor2"
                        ]
            in
            actual `shouldBe` expected


        it "adds the data and the constructors for all toplevel definitions" <|
            let
                module_ =
                    M.Module [ aCustomType, aFunction ]

                actual =
                    Context.build module_

                expectedConstructor constructorName =
                    ( constructorName
                    , TA.Custom (M.typeName aCustomType) []
                    )

                expected =
                    Map.fromList
                        [ expectedConstructor "ctor1"
                        , expectedConstructor "ctor2"
                        , ("functionName", TA.Int)
                        ]
            in
            actual `shouldBe` expected
