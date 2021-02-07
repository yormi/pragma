module Parser3.IdentifierSpec where

import Test.Hspec hiding (context)

import qualified AST3.Identifier as Identifier
import qualified Parser3.Error as E
import Parser3.Quote (Quote(..))
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
        describe "reference" <| do
            it "Fails on reserved string" <|
                let
                    parser =
                        Identifier.reference

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
                    parser =
                        Identifier.reference

                    source =
                        "PascalCase"

                    expected =
                        Identifier.referenceId (Quote aFilePath 1 1 1 10) source
                            |> Right
                in
                run parser source `shouldBe` expected


            it "Parses a camelCase string" <|
                let
                    parser =
                        Identifier.reference

                    source =
                        "camelCase"

                    expected =
                        Identifier.referenceId (Quote aFilePath 1 1 1 9) source
                            |> Right
                in
                run parser source `shouldBe` expected
