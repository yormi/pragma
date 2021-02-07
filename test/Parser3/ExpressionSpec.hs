module Parser3.ExpressionSpec where

import Test.Hspec hiding (context)

import qualified AST3.Expression as Expression
import qualified AST3.Identifier as Identifier
import Parser3.Position (Position(..))
import Parser3.Quote (Quote(..))
import qualified Parser3.Parser as Parser
import qualified Parser3.Expression as Expression
import qualified Utils.Either as Either
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
    describe "Expression Parser" <| do
        it "Fails on something that is not an expression" <|
            let
                sourceCode =
                    "???"
            in
            run sourceCode `shouldSatisfy` Either.isLeft


        describe "ParentesizedExpression" <| do
            it "Parses an expression in parentheses" <|
                let
                    source =
                        "(AnExpression)"

                    quote =
                        Quote aFilePath 1 2 1 13

                    expected =
                        Identifier.referenceId quote "AnExpression"
                            |> Expression.Reference
                            |> Right
                in
                run source `shouldBe` expected


            it "Parses an expression in parentheses with spaces" <|
                let
                    source =
                        "( AnExpression )"

                    quote =
                        Quote aFilePath 1 3 1 14

                    expected =
                        Identifier.referenceId quote "AnExpression"
                            |> Expression.Reference
                            |> Right
                in
                run source `shouldBe` expected



        describe "Reference" <| do
            it "Parses a reference starting with an upper case" <|
                let
                    source =
                        "PascalCase"

                    quote =
                        Quote aFilePath 1 1 1 10

                    expected =
                        Identifier.referenceId quote source
                            |> Expression.Reference
                            |> Right
                in
                run source `shouldBe` expected


            it "Parses a reference starting with a lower case" <|
                let
                    source =
                        "camelCase"

                    quote =
                        Quote aFilePath 1 1 1 9

                    expected =
                        Identifier.referenceId quote source
                            |> Expression.Reference
                            |> Right
                in
                run source `shouldBe` expected


        describe "If Then Else" <|
            let
                trueExpression quote =
                    Expression.TrueLiteral quote
                        |> Expression.Bool
                        |> Expression.Value
            in do
            it "Parses one word condition and alternatives" <|
                let
                    source =
                        [ "if True then"
                        , "    True"
                        , "else"
                        , "    True"
                        ]
                            |> String.mergeLines

                    expected =
                        Expression.If
                            (Position aFilePath 1 1)
                            (trueExpression <| buildQuote 1 4 1 7)
                            (trueExpression <| buildQuote 2 5 2 8)
                            (trueExpression <| buildQuote 4 5 4 8)
                            |> Right
                in
                run source `shouldBe` expected


            it "Parses application condition and alternatives" <|
                let
                    source =
                        [ "if f a then"
                        , "    f a"
                        , "else"
                        , "    f a"
                        ]
                            |> String.mergeLines

                    nameQuote line fromColumn =
                        buildQuote line fromColumn line fromColumn

                    application line fromColumn =
                        Expression.Application
                            { functionName =
                                Identifier.referenceId
                                    (nameQuote line fromColumn)
                                    "f"
                            , args = args line (fromColumn + 2)
                            }

                    args line fromColumn =
                        NonEmpty.build
                            (Identifier.referenceId
                                (nameQuote line fromColumn)
                                "a"
                                |> Expression.Reference
                            )
                            []

                    expected =
                        Expression.If
                            (Position aFilePath 1 1)
                            (application 1 4)
                            (application 2 5)
                            (application 4 5)
                            |> Right
                in
                run source `shouldBe` expected


        describe "Application" <| do
            it "Parses a one argument application" <|
                let
                    source =
                        "f a"

                    args =
                        NonEmpty.build
                            (Identifier.referenceId (buildQuote 1 3 1 3) "a"
                                |> Expression.Reference
                            )
                            []

                    expected =
                        Expression.Application
                            { functionName =
                                Identifier.referenceId (buildQuote 1 1 1 1) "f"
                            , args = args
                            }
                            |> Right
                in
                run source `shouldBe` expected


            it "Parses a constructor application" <|
                let
                    source =
                        "Constructor a"

                    args =
                        Identifier.referenceId (buildQuote 1 13 1 13) "a"
                            |> Expression.Reference
                            |> NonEmpty.singleton

                    expected =
                        Expression.Application
                            { functionName =
                                Identifier.referenceId (buildQuote 1 1 1 11)
                                    "Constructor"
                            , args = args
                            }
                            |> Right
                in
                run source `shouldBe` expected


            it "Parses a multiple arguments application" <|
                let
                    source =
                        "f a 3 (\"blah\") "

                    args =
                        NonEmpty.build
                            (Identifier.referenceId (buildQuote 1 3 1 3) "a"
                                |> Expression.Reference
                            )
                            [ Expression.Int (buildQuote 1 5 1 5) 3
                                    |> Expression.Value
                            , Expression.String (buildQuote 1 8 1 13) "blah"
                                    |> Expression.Value
                            ]

                    expected =
                        Expression.Application
                            { functionName =
                                Identifier.referenceId (buildQuote 1 1 1 1) "f"
                            , args = args
                            }
                            |> Right
                in
                run source `shouldBe` expected
