module Parser3.CombinatorSpec where

import Test.Hspec hiding (context)

import qualified Parser3.Combinator as C
import qualified Parser3.Model.Error as E
import Parser3.Model.Position (Position(..))
import Parser3.Model.Quote (Quote(..))
import qualified Parser3.Parser as Parser


spec :: Spec
spec =
    let
        aFilePath =
            "some/File/Path.pa"

        buildPosition =
            Position aFilePath

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


        describe "anyCharBut" <|
            let
                parser =
                    C.anyCharBut ['d', 'e', 'f']
            in do
            it "Parses given no unwanted chars" <|
                let
                    sourceCode =
                        "abc"

                    expected =
                        (buildPosition 1 1, 'a')
                in
                run parser sourceCode `shouldBe` Right expected


            it "Parses given unwanted chars" <|
                let
                    sourceCode =
                        "def"

                    expected =
                        Left <| E.NonDesiredChar (buildPosition 1 1) 'd'
                in
                run parser sourceCode `shouldBe` expected


            it "Fails given no remaining chars" <|
                let
                    sourceCode =
                        ""

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


        describe "oneOf" <| do
            it "Returns the successful parser value given the first parser succeed" <|
                let
                    sourceCode =
                        "if False then"

                    parser =
                        C.oneOf [ C.char 'i' , C.char '{' ]

                    expected =
                        Right <| buildPosition 1 1
                in do
                run parser sourceCode `shouldBe` expected


            it "Returns the successful parser value given the second parser succeeded" <|
                let
                    sourceCode =
                        "{ hello = \"world\" }"

                    parser =
                        C.oneOf [ C.char 'i' , C.char '{' ]

                    expected =
                        Right <| buildPosition 1 1
                in do
                run parser sourceCode `shouldBe` expected


            it "Fails with the last error given no parser consumed anything" <|
                let
                    sourceCode =
                        "+"

                    parser =
                        C.oneOf [ C.char 'i', C.char '{' ]

                    expected =
                        Left <| E.NotTheDesiredChar (buildPosition 1 1) '{'
                in do
                run parser sourceCode `shouldBe` expected


            describe "Fails with the error of the parser that consumed the most" <| do
                it "Given the first fails when 2 parsers are provided" <|
                    let
                        sourceCode =
                            "aa"

                        parser =
                            C.oneOf
                                [ do
                                    _ <- C.char 'a'
                                    C.char 'b'
                                , C.char '{'
                                ]

                        expected =
                            Left <| E.NotTheDesiredChar (buildPosition 1 2) 'b'
                    in do
                    run parser sourceCode `shouldBe` expected


                it "Given another than the first parser consumed the most and there is more than 2 parsers provided" <|
                    let
                        sourceCode =
                            "aaa"

                        parser =
                            C.oneOf
                                [ do
                                    _ <- C.char 'a'
                                    C.char '{'
                                , C.char '}'
                                , do
                                    _ <- C.char 'a'
                                    _ <- C.char 'a'
                                    C.char 'b'
                                ]

                        expected =
                            Left <| E.NotTheDesiredChar (buildPosition 1 3) 'b'
                    in do
                    run parser sourceCode `shouldBe` expected


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
                        |> Right
                in do
                run parser sourceCode `shouldBe` expected


            it "Returns an empty list given unable to parse even once" <|
                let
                    sourceCode =
                        ""

                    parser =
                        C.many C.anyChar

                    expected =
                        Right []
                in do
                run parser sourceCode `shouldBe` expected


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
                        |> Right
                in do
                run parser sourceCode `shouldBe` expected


        describe "maybe" <| do
            it "Returns Nothing given the given parser is not successful" <|
                let
                    sourceCode =
                        ""

                    parser =
                        C.maybe C.string

                    expected =
                        Right Nothing
                in do
                run parser sourceCode `shouldBe` expected


            it "Returns Just with the parsed value given a successful parser" <|
                let
                    sourceCode =
                        "yo"

                    parser =
                        C.maybe C.string

                    expected =
                        (Quote aFilePath 1 1 1 2, "yo")
                            |> Just
                            |> Right
                in do
                run parser sourceCode `shouldBe` expected
