module Test.Parser.AST.ValueSpec where

import Test.Hspec hiding (context)

import AST.Expression as Expression
import qualified Parser.Model.Error as Error
import qualified Parser.AST.Value as Value
import Parser.Model.Quote (Quote(..))
import qualified Parser.Parser as Parser
import qualified Utils.Either as Either
import qualified Utils.String as String


spec :: Spec
spec =
    let
        aFilePath = "some/File/Path.pa"

        run parser sourceCode =
            Parser.run aFilePath sourceCode parser
    in
    describe "Value Parser" <| do
        describe "Invalid Value" <|
            it "Fails given no valid value" <|
                let
                    sourceCode =
                        "''"

                in
                run Value.parser sourceCode `shouldSatisfy` Either.isLeft


        describe "char" <| do
            it "Fails given no char in the single quote" <|
                let
                    sourceCode =
                        "''"
                in
                run Value.parser sourceCode `shouldSatisfy` Either.isLeft


            it "Parses a char literal" <|
                let
                    sourceCode =
                        "'c'"

                    expected =
                        Expression.Char (Quote aFilePath 1 1 1 3) 'c'
                            |> Right
                in
                run Value.parser sourceCode `shouldBe` expected


            it "Parses a single quote char" <|
                let
                    sourceCode =
                        "'\\''"

                    expected =
                        Expression.Char (Quote aFilePath 1 1 1 4) '\''
                            |> Right
                in
                run Value.parser sourceCode `shouldBe` expected


            it "Parses a newline char" <|
                let
                    sourceCode =
                        "'\\n'"

                    expected =
                        Expression.Char (Quote aFilePath 1 1 1 4) '\n'
                            |> Right
                in
                run Value.parser sourceCode `shouldBe` expected


            it "Parses a tab char" <|
                let
                    sourceCode =
                        "'\\t'"

                    expected =
                        Expression.Char (Quote aFilePath 1 1 1 4) '\t'
                            |> Right
                in
                run Value.parser sourceCode `shouldBe` expected


        describe "bool" <| do
            it "Parses False" <|
                let
                    sourceCode =
                        "False"

                    expected =
                        Quote aFilePath 1 1 1 5
                            |> FalseLiteral
                            |> Expression.Bool
                            |> Right
                in
                run Value.parser sourceCode `shouldBe` expected


            it "Parses True" <|
                let
                    sourceCode =
                        "True"

                    expected =
                        Quote aFilePath 1 1 1 4
                            |> TrueLiteral
                            |> Expression.Bool
                            |> Right
                in
                run Value.parser sourceCode `shouldBe` expected


        describe "int" <| do
            it "Parses a positive int" <|
                let
                    sourceCode =
                        "123"

                    expected =
                        Expression.Int (Quote aFilePath 1 1 1 3) 123
                            |> Right
                in
                run Value.parser sourceCode `shouldBe` expected


            it "Parses a negative int" <|
                let
                    sourceCode =
                        "-004"

                    expected =
                        Expression.Int (Quote aFilePath 1 1 1 4) (-4)
                            |> Right
                in
                run Value.parser sourceCode `shouldBe` expected


            it "Parses zero" <|
                let
                    sourceCode =
                        "0"

                    expected =
                        Expression.Int (Quote aFilePath 1 1 1 1) 0
                            |> Right
                in
                run Value.parser sourceCode `shouldBe` expected


        describe "string" <| do
            it "Fails given a multiline string" <|
                let
                    sourceCode =
                        [ "\"two line"
                        , "    string\""
                        ]
                            |> String.mergeLines

                    expected =
                        Quote aFilePath 1 1 1 10
                            |> Error.StringMustBeOnSingleLine
                            |> Left
                in
                run Value.parser sourceCode `shouldBe` expected


            it "Parses a string literal" <|
                let
                    sourceCode =
                        "\"two words\""

                    expected =
                        Expression.String (Quote aFilePath 1 1 1 11) "two words"
                            |> Right
                in
                run Value.parser sourceCode `shouldBe` expected


            it "Parses a string literal with an escaped double quote in it" <|
                let
                    sourceCode =
                        "\"Quoted \\\"Word\\\"\""

                    expected =
                        Expression.String (Quote aFilePath 1 1 1 17)
                            "Quoted \"Word\""
                            |> Right
                in
                run Value.parser sourceCode `shouldBe` expected


            it "Parses a string literal with a new line in it" <|
                let
                    sourceCode =
                        "\"First Line\\nSecond Line\""

                    expected =
                        Expression.String (Quote aFilePath 1 1 1 25)
                            "First Line\nSecond Line"
                            |> Right
                in
                run Value.parser sourceCode `shouldBe` expected


            it "Parses a string literal with a tab in it" <|
                let
                    sourceCode =
                        "\"TopLevel\\tIndented Line\""

                    expected =
                        Expression.String (Quote aFilePath 1 1 1 25)
                            "TopLevel\tIndented Line"
                            |> Right
                in
                run Value.parser sourceCode `shouldBe` expected
