module Parser3.IdentifierSpec where

import Test.Hspec hiding (context)

import qualified AST3.Identifier as Identifier
import qualified Parser3.Model.Error as E
import Parser3.Model.Quote (Quote(..))
import qualified Parser3.Parser as Parser
import qualified Parser3.Identifier as Identifier


spec :: Spec
spec =
    let
        aFilePath = "some/File/Path.pa"

        run parser sourceCode =
            Parser.run aFilePath sourceCode parser
    in
    describe "Identifier Parser" <| do
        describe "reference" <|
            let
                parser =
                    Identifier.reference
            in do
            it "Fails on reserved string" <|
                let
                    source =
                        "if"

                    expected =
                        E.IdentifierCantBeAReservedWord
                            (Quote aFilePath 1 1 1 2)
                            "if"
                            |> Left
                in
                run parser source `shouldBe` expected


            it "Parses a PascalCase string" <|
                let
                    source =
                        "PascalCase"

                    expected =
                        Identifier.referenceId (Quote aFilePath 1 1 1 10) source
                            |> Right
                in
                run parser source `shouldBe` expected


            it "Parses a camelCase string" <|
                let
                    source =
                        "camelCase"

                    expected =
                        Identifier.referenceId (Quote aFilePath 1 1 1 9) source
                            |> Right
                in
                run parser source `shouldBe` expected


        describe "data_" <|
            let
                parser =
                    Identifier.data_
            in do
            it "Fails on reserved string" <|
                let

                    source =
                        "if"

                    expected =
                        E.IdentifierCantBeAReservedWord
                            (Quote aFilePath 1 1 1 2)
                            "if"
                            |> Left
                in
                run parser source `shouldBe` expected


            it "Fails on a PascalCase string" <|
                let
                    source =
                        "PascalCase"

                    expected =
                        Quote aFilePath 1 1 1 10
                            |> E.DataIdMustStartWithLowerCase
                            |> Left
                in
                run parser source `shouldBe` expected


            it "Parses a camelCase string" <|
                let
                    source =
                        "camelCase"

                    expected =
                        Identifier.dataId (Quote aFilePath 1 1 1 9) source
                in
                case expected of
                    Just e ->
                        run parser source `shouldBe` Right e

                    Nothing ->
                        expectationFailure "Bad test"
