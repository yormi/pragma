module Test.Parser.AST.Expression.LetInSpec where

import Test.Hspec hiding (context)

import qualified Test.Parser.Utils as Utils
import qualified Test.Utils as TestUtils

import qualified AST.Expression as Expression
import qualified AST.Identifier as Identifier
import Parser.Model.Position (Position(..))
import Parser.Model.Quote (Quote(..))
import qualified Parser.Parser as Parser
import qualified Parser.AST.Expression as Expression
import qualified Utils.NonEmpty as NonEmpty
import qualified Utils.String as String


spec :: Spec
spec =
    let
        aFilePath = "some/File/Path.pa"

        buildQuote lineFrom columnFrom lineTo lineColumn =
            Quote aFilePath lineFrom columnFrom lineTo lineColumn

        run sourceCode =
            Expression.expressionParser
                |> Parser.run aFilePath sourceCode
    in
    describe "Let..In Parser" <| do
        it "Parses one definition and body" <|
            let
                source =
                    [ " let"
                    , "    x ="
                    , "        True"
                    , "in"
                    , "True"
                    ]
                        |> String.mergeLines


                expected definitionName =
                    Expression.LetIn
                        (Utils.position 1 2)
                        ( Expression.SimpleDefinition definitionName (Utils.true 3 9)
                            |> NonEmpty.singleton
                        )
                        (Utils.true 5 1)
                        |> Right
            in do
            definitionName <-
                Identifier.dataId (buildQuote 2 5 2 5) "x"
                    |> TestUtils.assumeJust
            run source `shouldBe` expected definitionName


        it "Parses two definitions and body" <|
            let
                source =
                    [ " let"
                    , "    x ="
                    , "        True"
                    , ""
                    , "    y ="
                    , "        True"
                    , "in"
                    , "True"
                    ]
                        |> String.mergeLines


                expected firstDefinition secondDefinition =
                    Expression.LetIn
                        (Position aFilePath 1 2)
                        ( NonEmpty.build
                            ( Expression.SimpleDefinition firstDefinition
                                (Utils.true 3 9)
                            )
                            [ Expression.SimpleDefinition secondDefinition
                                (Utils.true 6 9)
                            ]
                        )
                        (Utils.true 8 1)
                        |> Right
            in do
            firstDefinition <- Utils.dataId 2 5 "x"
            secondDefinition <- Utils.dataId 5 5 "y"
            run source `shouldBe` expected firstDefinition secondDefinition


        describe "With first argument as" <|
            let
                runForFirstDefinition source =
                    run source
                        |> map Expression.definitions
                        |> map NonEmpty.head
                        |> map
                            (\(Expression.SimpleDefinition _ expression) ->
                                expression
                            )
            in do
            it "a one-line application" <|
                let
                    source =
                        [ "let"
                        , "    x ="
                        , "        ff aa"
                        , ""
                        , "    y ="
                        , "        f a"
                        , "in"
                        , "f a"
                        ]
                            |> String.mergeLines

                    expected =
                        Expression.Application
                            { functionName =
                                Identifier.referenceId (buildQuote 3 9 3 10)
                                    "ff"
                            , args = NonEmpty.singleton
                                (Utils.reference 3 12 "aa")
                            }
                            |> Right
                in
                runForFirstDefinition source `shouldBe` expected



            it "a multi-line application" <|
                let
                    source =
                        [ "let"
                        , "    x ="
                        , "        ff aa"
                        , "            bb"
                        , ""
                        , "    y ="
                        , "        f a"
                        , "in"
                        , "f a"
                        ]
                            |> String.mergeLines

                    expected =
                        Expression.Application
                            { functionName =
                                Identifier.referenceId (buildQuote 3 9 3 10)
                                    "ff"
                            , args = NonEmpty.build
                                (Utils.reference 3 12 "aa")
                                [Utils.reference 4 13 "bb"]
                            }
                            |> Right
                in do
                runForFirstDefinition source `shouldBe` expected


        describe "With last argument as" <|
            let
                runForFirstDefinition source =
                    run source
                        |> map Expression.definitions
                        |> map NonEmpty.last
                        |> map
                            (\(Expression.SimpleDefinition _ expression) ->
                                expression
                            )
            in do
            it "a one-line application" <|
                let
                    source =
                        [ "let"
                        , "    x ="
                        , "        f a"
                        , ""
                        , "    y ="
                        , "        ff aa"
                        , "in"
                        , "f a"
                        ]
                            |> String.mergeLines

                    expected =
                        Expression.Application
                            { functionName =
                                Identifier.referenceId (buildQuote 6 9 6 10)
                                    "ff"
                            , args =
                                NonEmpty.singleton
                                    (Utils.reference 6 12 "aa")
                            }
                            |> Right
                in
                runForFirstDefinition source `shouldBe` expected



            it "a multi-line application" <|
                let
                    source =
                        [ "let"
                        , "    x ="
                        , "        f a"
                        , ""
                        , "    y ="
                        , "        ff aa"
                        , "            bb"
                        , "in"
                        , "f a"
                        ]
                            |> String.mergeLines

                    expected =
                        Expression.Application
                            { functionName =
                                Identifier.referenceId (buildQuote 6 9 6 10)
                                    "ff"
                            , args =
                                NonEmpty.build
                                    (Utils.reference 6 12 "aa")
                                    [Utils.reference 7 13 "bb"]
                            }
                            |> Right
                in do
                runForFirstDefinition source `shouldBe` expected
