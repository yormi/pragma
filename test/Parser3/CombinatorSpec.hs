module Parser3.CombinatorSpec where

import Test.Hspec hiding (context)

import qualified Parser3.Combinator as C
import qualified Parser3.Error as E
import Parser3.Position (Position(..))
import Parser3.Quote (Quote(..))
import qualified Parser3.Parser as Parser


spec :: Spec
spec =
    let
        aFilePath =
            "some/File/Path.pa"

        buildPosition =
            Position aFilePath

        testSuccess parser sourceCode expected =
            test parser sourceCode (Right expected)

        testFailure parser sourceCode expected =
            test parser sourceCode (Left expected)

        test parser sourceCode expected =
            run parser sourceCode
                |> (`shouldBe` expected)

        run parser sourceCode =
            Parser.run aFilePath sourceCode parser
    in
    describe "Parser Combinators" <| do
        describe "anyChar" <| do
            it "Parses once" <|
                let
                    sourceCode =
                        "abc"

                    expected =
                        (buildPosition 1 1, 'a')
                in
                run C.anyChar sourceCode `shouldBe` Right expected


            it "Progress to next char after first one has been parsed" <|
                let
                    sourceCode =
                        "abc"

                    parser = do
                        a <- C.anyChar
                        b <- C.anyChar
                        return (a, b)

                    expected =
                        ( (buildPosition 1 1, 'a')
                        , (buildPosition 1 2, 'b')
                        )
                        |> Right
                in do
                run parser sourceCode `shouldBe` expected


            it "Fails given no remaining chars" <|
                let
                    sourceCode =
                        ""

                    parser =
                        C.anyChar

                    expected =
                        Left E.EndOfFileReached
                in do
                run parser sourceCode `shouldBe` expected


        describe "string" <| do
            it "Parses first word given splitted by space" <|
                let
                    sourceCode =
                        "two words"

                    expected =
                        Right (Quote aFilePath 1 1 1 3, "two")
                in
                run C.string sourceCode `shouldBe` expected

            it "Parses first word given splitted by closing parenthesized" <|
                let
                    sourceCode =
                        "two)words"

                    expected =
                        Right (Quote aFilePath 1 1 1 3, "two")
                in
                run C.string sourceCode `shouldBe` expected

            it "Parses first word given splitted by closing square bracket" <|
                let
                    sourceCode =
                        "two]words"

                    expected =
                        Right (Quote aFilePath 1 1 1 3, "two")
                in
                run C.string sourceCode `shouldBe` expected


            it "Parses two words" <|
                let
                    sourceCode =
                        "two words"

                    parser = do
                        a <- C.string
                        _ <- C.space
                        b <- C.string
                        return (a, b)

                    expected =
                        ( (Quote aFilePath 1 1 1 3, "two")
                        , (Quote aFilePath 1 5 1 9, "words")
                        )
                            |> Right
                in
                run parser sourceCode `shouldBe` expected


        describe "oneOf" <|
            let
                aPosition =
                    Position aFilePath 1 1

                anError =
                    E.ThisIsABug aPosition "anError"
            in do
            it "Returns the successful parser value given the first parser succeed" <|
                let
                    sourceCode =
                        "if False then"

                    parser =
                        C.oneOf anError [ C.char 'i' , C.char '{' ]

                    expected =
                        buildPosition 1 1
                in do
                testSuccess parser sourceCode expected


            it "Returns the successful parser value given the second parser succeeded" <|
                let
                    sourceCode =
                        "{ hello = \"world\" }"

                    parser =
                        C.oneOf anError [ C.char 'i' , C.char '{' ]

                    expected =
                        buildPosition 1 1
                in do
                testSuccess parser sourceCode expected


            it "Fails with the provided error given no parser succeeds" <|
                let
                    sourceCode =
                        "+"

                    parser =
                        C.oneOf anError [ C.char 'i' , C.char '{' ]

                    expected =
                        anError
                in do
                testFailure parser sourceCode expected


        describe "many" <| do
            it "Returns a list of the parsed value" <|
                let
                    sourceCode =
                        "a b c d"

                    parser =
                        C.many <| do
                            C.someSpace
                            C.anyChar

                    expected =
                        [ (buildPosition 1 1, 'a')
                        , (buildPosition 1 3, 'b')
                        , (buildPosition 1 5, 'c')
                        , (buildPosition 1 7, 'd')
                        ]
                in do
                testSuccess parser sourceCode expected


            it "Returns an empty list given unable to parse even once" <|
                let
                    sourceCode =
                        ""

                    parser =
                        C.many C.anyChar

                    expected =
                        []
                in do
                testSuccess parser sourceCode expected


        describe "atLeastOne" <| do
            it "Returns a list of the parsed value" <|
                let
                    sourceCode =
                        "a b c d"

                    parser =
                        C.many <| do
                            C.someSpace
                            C.anyChar

                    expected =
                        [ (buildPosition 1 1, 'a')
                        , (buildPosition 1 3, 'b')
                        , (buildPosition 1 5, 'c')
                        , (buildPosition 1 7, 'd')
                        ]
                in do
                testSuccess parser sourceCode expected


        describe "maybe" <| do
            it "Returns Nothing given the given parser is not successful" <|
                let
                    sourceCode =
                        ""

                    parser =
                        C.maybe C.string

                    expected =
                        Nothing
                in do
                testSuccess parser sourceCode expected


            it "Returns Just with the parsed value given a successful parser" <|
                let
                    sourceCode =
                        "yo"

                    parser =
                        C.maybe C.string

                    expected =
                        (Quote aFilePath 1 1 1 2, "yo")
                            |> Just
                in do
                testSuccess parser sourceCode expected
