module Test.Parser.AST.ExpressionSpec where

import Test.Hspec hiding (context)

import qualified Test.Parser.Utils as Utils

import qualified AST.Expression as Expression
import qualified AST.Identifier as Identifier
import qualified Parser.AST.Expression as Expression
import qualified Utils.Either as Either
import qualified Utils.NonEmpty as NonEmpty
import qualified Utils.String as String


spec :: Spec
spec =
    let
        run sourceCode =
            Utils.run sourceCode Expression.expressionParser
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
                        Utils.quote 1 2 1 13

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
                        Utils.quote 1 3 1 14

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
                        Utils.quote 1 1 1 10

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
                        Utils.quote 1 1 1 9

                    expected =
                        Identifier.referenceId quote source
                            |> Expression.Reference
                            |> Right
                in
                run source `shouldBe` expected


        describe "If Then Else" <| do
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
                            (Utils.position 1 1)
                            (Utils.true 1 4)
                            (Utils.true 2 5)
                            (Utils.true 4 5)
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
                        Utils.quote line fromColumn line fromColumn

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
                            (Utils.position 1 1)
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
                            (Identifier.referenceId (Utils.quote 1 3 1 3) "a"
                                |> Expression.Reference
                            )
                            []

                    expected =
                        Expression.Application
                            { functionName =
                                Identifier.referenceId (Utils.quote 1 1 1 1) "f"
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
                        Identifier.referenceId (Utils.quote 1 13 1 13) "a"
                            |> Expression.Reference
                            |> NonEmpty.singleton

                    expected =
                        Expression.Application
                            { functionName =
                                Identifier.referenceId (Utils.quote 1 1 1 11)
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
                            (Identifier.referenceId (Utils.quote 1 3 1 3) "a"
                                |> Expression.Reference
                            )
                            [ Expression.Int (Utils.quote 1 5 1 5) 3
                                    |> Expression.Value
                            , Expression.String (Utils.quote 1 8 1 13) "blah"
                                    |> Expression.Value
                            ]

                    expected =
                        Expression.Application
                            { functionName =
                                Identifier.referenceId (Utils.quote 1 1 1 1) "f"
                            , args = args
                            }
                            |> Right
                in
                run source `shouldBe` expected
