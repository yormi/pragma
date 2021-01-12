module Parser2.ModelSpec where

import Test.Hspec hiding (context)

import Parser2.Model


spec :: Spec
spec =
    let
        aFilePath = "some/File/Path.pa"

        testSuccess parser sourceCode expected =
            test parser sourceCode (Right expected)

        testFailure parser sourceCode expected =
            expected
                |> RawError
                |> Left
                |> test parser sourceCode

        test parser sourceCode expected =
            parser
                |> run aFilePath sourceCode
                |> (`shouldBe` expected)
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

                    parser =
                        anyChar

                    expected =
                        EndOfFileReached
                in do
                testFailure parser sourceCode expected


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
                testSuccess parser sourceCode expected


            it "Fails given currently on a space" <|
                let
                    sourceCode =
                        "two words"

                    parser = do
                        a <- string
                        b <- string
                        return (a, b)

                    expected =
                        Position aFilePath 1 4
                            |> StringExpected

                in
                testFailure parser sourceCode expected


        describe "identifier" <| do
            it "Parses once given all legal id characters" <|
                let
                    sourceCode =
                        "two words"

                    expected =
                        Parsed
                            (Quote aFilePath 1 1 1 3)
                            "two"
                in
                testSuccess identifier sourceCode expected


            it "Fails on a space" <|
                let
                    sourceCode =
                        "two words"

                    parser = do
                        a <- identifier
                        b <- identifier
                        return (a, b)

                    expected =
                        Position aFilePath 1 4
                            |> StringExpected
                in
                testFailure parser sourceCode expected


            it "Fails given a string with characters that are not valid identifier characters" <|
                let
                    sourceCode =
                        "two$ words"

                    parser = do
                        a <- identifier
                        b <- identifier
                        return (a, b)

                    expected =
                        [ (Position aFilePath 1 4, '$') ]
                            |> InvalidCharactersInIdentifier
                in
                testFailure parser sourceCode expected


            it "Fails given the provided word is in the reserved words" <|
                let
                    reservedWord =
                        "else"

                    sourceCode =
                        reservedWord

                    parser =
                        identifier

                    expected =
                        IdentifierCantBeAReservedWord
                            (Quote aFilePath 1 1 1 4)
                            reservedWord
                in do
                testFailure parser sourceCode expected


        describe "reserved" <| do
            it "Returns the quote given the string is the one provided" <|
                let
                    sourceCode =
                        "if True then"

                    parser =
                        reserved "if"

                    expected =
                        Quote aFilePath 1 1 1 2
                in do
                testSuccess parser sourceCode expected


            it "Fails given the provided word is not in the reserved words" <|
                let
                    typoed =
                        "esle"

                    sourceCode =
                        typoed

                    parser =
                        reserved typoed

                    expected =
                        ThisIsABug "esle is not a reserved word"
                in do
                testFailure parser sourceCode expected


            it "Fails given the read string was not the one desired" <|
                let
                    sourceCode =
                        "else"

                    reservedWord =
                        "if"

                    parser =
                        reserved reservedWord

                    expected =
                        ReservedWordExpected
                            (Quote aFilePath 1 1 1 4)
                            reservedWord
                in do
                testFailure parser sourceCode expected


        describe "operator" <| do
            it "Returns the quote given the string is the one provided" <|
                let
                    sourceCode =
                        "=="

                    parser =
                        operator "=="

                    expected =
                        Quote aFilePath 1 1 1 2
                in do
                testSuccess parser sourceCode expected


            it "Fails given the provided word is not in the operator list" <|
                let
                    typoed =
                        "!="

                    sourceCode =
                        typoed

                    parser =
                        operator typoed

                    expected =
                        ThisIsABug "!= is not an operator"
                in do
                testFailure parser sourceCode expected


            it "Fails given the read string was not the one desired" <|
                let
                    sourceCode =
                        "="

                    reservedWord =
                        ":"

                    parser =
                        operator reservedWord

                    expected =
                        OperatorExpected
                            (Quote aFilePath 1 1 1 1)
                            reservedWord
                in do
                testFailure parser sourceCode expected


        describe "oneOf" <|
            let
                anError =
                    ThisIsABug "anError"
            in do
            it "Returns the successful parser value given the first parser succeed" <|
                let
                    sourceCode =
                        "if False then"

                    parser =
                        oneOf
                            (RawError anError)
                            [ reserved "if"
                            , operator "{"
                            ]

                    expected =
                        Quote aFilePath 1 1 1 2
                in do
                testSuccess parser sourceCode expected


            it "Returns the successful parser value given the second parser succeed" <|
                let
                    sourceCode =
                        "{ hello = \"world\" }"

                    parser =
                        oneOf
                            (RawError anError)
                            [ reserved "if"
                            , operator "{"
                            ]

                    expected =
                        Quote aFilePath 1 1 1 1
                in do
                testSuccess parser sourceCode expected


            it "Fails with the provided error given no parser succeeds" <|
                let
                    sourceCode =
                        "+"

                    parser =
                        oneOf
                            (RawError anError)
                            [ reserved "if"
                            , operator "{"
                            ]

                    expected =
                        anError
                in do
                testFailure parser sourceCode expected
