module Parser3.Expression.LetInSpec where

import Test.Hspec hiding (context)

import Sample.AST (trueExpression)
import qualified TestUtils as T

import qualified AST3.Expression as Expression
import qualified AST3.Identifier as Identifier
import Parser3.Model.Position (Position(..))
import Parser3.Model.Quote (Quote(..))
import qualified Parser3.Parser as Parser
import qualified Parser3.Expression as Expression
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
                        (Position aFilePath 1 2)
                        ( NonEmpty.singleton <|
                            Expression.SimpleDefinition
                                definitionName
                                (trueExpression <| buildQuote 3 9 3 12)
                        )
                        (trueExpression <| buildQuote 5 1 5 4)
                        |> Right
            in do
            definitionName <-
                Identifier.dataId (buildQuote 2 5 2 5) "x"
                    |> T.assumeJust
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
                            ( Expression.SimpleDefinition
                                firstDefinition
                                (trueExpression <| buildQuote 3 9 3 12)
                            )
                            [ Expression.SimpleDefinition
                                secondDefinition
                                (trueExpression <| buildQuote 6 9 6 12)
                            ]
                        )
                        (trueExpression <| buildQuote 8 1 8 4)
                        |> Right
            in do
            firstDefinition <-
                Identifier.dataId (buildQuote 2 5 2 5) "x"
                    |> T.assumeJust
            secondDefinition <-
                Identifier.dataId (buildQuote 5 5 5 5) "y"
                    |> T.assumeJust
            run source `shouldBe` expected firstDefinition secondDefinition


        describe "With first argument as" <|
            let
                reference quote str =
                    Identifier.referenceId quote str
                        |> Expression.Reference

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
                                (reference (buildQuote 3 12 3 13) "aa")
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
                                (reference (buildQuote 3 12 3 13) "aa")
                                [reference (buildQuote 4 13 4 14) "bb"]
                            }
                            |> Right
                in do
                runForFirstDefinition source `shouldBe` expected


        describe "With last argument as" <|
            let
                reference quote str =
                    Identifier.referenceId quote str
                        |> Expression.Reference

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
                                    (reference (buildQuote 6 12 6 13) "aa")
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
                                    (reference (buildQuote 6 12 6 13) "aa")
                                    [reference (buildQuote 7 13 7 14) "bb"]
                            }
                            |> Right
                in do
                runForFirstDefinition source `shouldBe` expected
