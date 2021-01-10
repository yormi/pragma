module Parser2.ModelSpec where

import Test.Hspec hiding (context)

import Control.Monad (fail)

import Parser.Error hiding (RawError)
import Parser2.Model


spec :: Spec
spec =
    let
        aFilePath = "some/File/Path.pa"

        testSuccess parser sourceCode expected =
            parser
                |> run aFilePath sourceCode
                |> (`shouldBe` Right expected)
    in
    fdescribe "Parser" <| do
        describe "anyChar" <| do
            it "Parses once" <|
                let
                    sourceCode =
                        "abc"

                    expected =
                        Parsed (Quote aFilePath 1 1 1 1) 'a'
                in
                testSuccess anyChar sourceCode expected


            it "Progress to next char after first one has been parsed" <|
                let
                    sourceCode =
                        "abc"

                    parser = do
                        a <- anyChar
                        b <- anyChar
                        return (a, b)

                    expected =
                        ( Parsed (Quote aFilePath 1 1 1 1) 'a'
                        , Parsed (Quote aFilePath 1 2 1 2) 'b'
                        )
                in do
                testSuccess parser sourceCode expected


            it "Fails given no remaining chars" <|
                let
                    sourceCode =
                        ""

                    parser = do
                        a <- anyChar
                        b <- anyChar
                        return (a, b)

                    expected =
                        RawError EndOfFileReached
                            |> Left
                in do
                parser
                    |> run aFilePath sourceCode
                    |> (`shouldBe` expected)


        describe "string" <| do
            it "Parses once" <|
                let
                    sourceCode =
                        "two words"

                    expected =
                        Parsed
                            (Quote aFilePath 1 1 1 3)
                            "two"
                in
                string
                    |> run aFilePath sourceCode
                    |> (`shouldBe` Right expected)


            it "Parses two words" <|
                let
                    sourceCode =
                        "two words"

                    parser = do
                        a <- string
                        space
                        b <- string
                        return (a, b)

                    expected =
                        ( Parsed
                            (Quote aFilePath 1 1 1 3)
                            "two"
                        , Parsed
                            (Quote aFilePath 1 5 1 9)
                            "words"
                        )
                in
                parser
                    |> run aFilePath sourceCode
                    |> (`shouldBe` Right expected)
