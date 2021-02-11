module Parser3.LexemeSpec where

import Test.Hspec hiding (context)

import qualified Parser3.Combinator as C
import qualified Parser3.Model.Error as E
import qualified Parser3.Lexeme as Lexeme
import Parser3.Model.Position (Position(..))
import Parser3.Model.Quote (Quote(..))
import qualified Parser3.Parser as Parser


spec :: Spec
spec =
    let
        aFilePath = "some/File/Path.pa"

        run parser sourceCode =
            Parser.run aFilePath sourceCode parser
    in
    describe "Parser Lexeme" <| do
        describe "identifier" <| do
            it "Parses once given all legal id characters" <|
                let
                    sourceCode =
                        "two words"

                    expected =
                        Right (Quote aFilePath 1 1 1 3, "two")
                in
                run Lexeme.identifier sourceCode `shouldBe` expected


            it "consumes the spaces before a word then parses the identifier" <|
                let
                    sourceCode =
                        "  someId"

                    parser =
                        Lexeme.identifier

                    expected =
                        Right (Quote aFilePath 1 3 1 8, "someId")
                in
                run parser sourceCode `shouldBe` expected


            it "Fails given a string with characters that are not valid identifier characters" <|
                let
                    sourceCode =
                        "two$ words"

                    parser = do
                        a <- Lexeme.identifier
                        b <- Lexeme.identifier
                        return (a, b)

                    expected =
                        [ (Position aFilePath 1 4, '$') ]
                            |> E.InvalidCharactersInIdentifier
                            |> Left
                in
                run parser sourceCode `shouldBe` expected


            it "Fails given the provided word is in the reserved words" <|
                let
                    reservedWord =
                        "else"

                    sourceCode =
                        reservedWord

                    parser =
                        Lexeme.identifier

                    expected =
                        E.IdentifierCantBeAReservedWord
                            (Quote aFilePath 1 1 1 4)
                            reservedWord
                            |> Left
                in do
                run parser sourceCode `shouldBe` expected


        describe "reserved" <| do
            it "Returns the quote given the string is the one provided" <|
                let
                    sourceCode =
                        "if True then"

                    parser =
                        Lexeme.reserved "if"

                    expected =
                        Quote aFilePath 1 1 1 2
                            |> Right
                in do
                run parser sourceCode `shouldBe` expected


            it "Fails given the provided word is not in the reserved words" <|
                let
                    typoed =
                        "esle"

                    sourceCode =
                        typoed

                    parser =
                        Lexeme.reserved typoed

                    expected =
                        E.ThisIsABug
                            (Position aFilePath 1 1)
                            "esle is not a reserved word"
                            |> Left
                in do
                run parser sourceCode `shouldBe` expected


            it "Fails given the read string was not the one desired" <|
                let
                    sourceCode =
                        "else"

                    reservedWord =
                        "if"

                    parser =
                        Lexeme.reserved reservedWord

                    expected =
                        E.ReservedWordExpected
                            (Quote aFilePath 1 1 1 4)
                            reservedWord
                            |> Left
                in do
                run parser sourceCode `shouldBe` expected


        describe "operator" <| do
            it "Returns the quote given the string is the one provided" <|
                let
                    sourceCode =
                        "=="

                    parser =
                        Lexeme.operator "=="

                    expected =
                        Right <| Quote aFilePath 1 1 1 2
                in do
                run parser sourceCode `shouldBe` expected


            it "Returns the quote even if there is spaces in front of the operator" <|
                let
                    sourceCode =
                        " =="

                    parser =
                        Lexeme.operator "=="

                    expected =
                        Quote aFilePath 1 2 1 3
                            |> Right
                in do
                run parser sourceCode `shouldBe` expected


            it "Fails given the provided word is not in the operator list" <|
                let
                    typoed =
                        "!="

                    sourceCode =
                        typoed

                    parser =
                        Lexeme.operator typoed

                    expected =
                        E.ThisIsABug
                            (Position aFilePath 1 1)
                            "!= is not an operator"
                            |> Left
                in do
                run parser sourceCode `shouldBe` expected


            it "Fails given the read string was not the one desired" <|
                let
                    sourceCode =
                        "="

                    reservedWord =
                        ":"

                    parser =
                        Lexeme.operator reservedWord

                    expected =
                        E.OperatorExpected
                            (Position aFilePath 1 1)
                            reservedWord
                            |> Left
                in do
                run parser sourceCode `shouldBe` expected


            it "Fails as a bug given the provided string is empty" <|
                let
                    sourceCode =
                        "="

                    parser =
                        Lexeme.operator ""

                    expected =
                        E.ThisIsABug
                            (Position aFilePath 1 1)
                            "The required operator should not be empty"
                            |> Left
                in do
                run parser sourceCode `shouldBe` expected


        describe "parenthesized" <| do
            it "Parses successfully given no spaces" <|
                let
                    sourceCode =
                        "(yo)"

                    parser =
                        Lexeme.parenthesized C.string

                    expected =
                        Right (Quote aFilePath 1 2 1 3, "yo")
                in do
                run parser sourceCode `shouldBe` expected


            it "Parses successfully given spaces before the opening parenthesis" <|
                let
                    sourceCode =
                        "  (yo)"

                    parser =
                        Lexeme.parenthesized C.string

                    expected =
                        Right (Quote aFilePath 1 4 1 5, "yo")
                in do
                run parser sourceCode `shouldBe` expected


            it "Parses successfully given spaces after the opening parenthesis" <|
                let
                    sourceCode =
                        "(  yo)"

                    parser =
                        Lexeme.parenthesized C.string

                    expected =
                        Right (Quote aFilePath 1 4 1 5, "yo")
                in do
                run parser sourceCode `shouldBe` expected


            it "Parses successfully given spaces before the closing parenthesis" <|
                let
                    sourceCode =
                        "(yo  )"

                    parser =
                        Lexeme.parenthesized C.string

                    expected =
                        Right (Quote aFilePath 1 2 1 3, "yo")
                in do
                run parser sourceCode `shouldBe` expected


            it "Fails given no opening parenthesis" <|
                let
                    sourceCode =
                        "yo)"

                    parser =
                        Lexeme.parenthesized C.string

                    expected =
                        E.NotTheDesiredChar (Position aFilePath 1 1) '('
                            |> Left
                in do
                run parser sourceCode `shouldBe` expected


            it "Fails given no closing parenthesis" <|
                let
                    sourceCode =
                        "(yo bob"

                    parser =
                        Lexeme.parenthesized C.string

                    expected =
                        E.NotTheDesiredChar (Position aFilePath 1 5) ')'
                            |> Left
                in do
                run parser sourceCode `shouldBe` expected
