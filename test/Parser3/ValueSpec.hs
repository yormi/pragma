module Parser3.ValueSpec where

import Test.Hspec hiding (context)

import AST3.Expression as Expression
import qualified Parser3.Error as E
import qualified Parser3.Value as Value
import Parser3.Position (Position(..))
import Parser3.Quote (Quote(..))
import qualified Parser3.Parser as Parser


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
                        "???"

                    expected =
                        E.ValueExpected (Position aFilePath 1 1)
                            |> Left
                in
                run Value.parser sourceCode `shouldBe` expected


        describe "string" <| do
            it "Parses a string literal" <|
                let
                    sourceCode =
                        "\"two words\""

                    expected =
                        Expression.String (Quote aFilePath 1 1 1 11) "two words"
                            |> Right
                in
                run Value.parser sourceCode `shouldBe` expected


            it "Parses a string literal with an escape double quote in it" <|
                let
                    sourceCode =
                        "\"Quoted \\\"Word\\\"\""

                    expected =
                        Expression.String (Quote aFilePath 1 1 1 17)
                            "Quoted \"Word\""
                            |> Right
                in
                run Value.parser sourceCode `shouldBe` expected
