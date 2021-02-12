module Parser3.IndentationSpec where

import Test.Hspec hiding (context)

import qualified Parser3.Combinator as C
import qualified Parser3.Model.Error as E
import Parser3.Model.Position (Position(..))
import qualified Parser3.Parser as Parser
import qualified Parser3.Indentation as Indentation
import qualified Utils.String as String


spec :: Spec
spec =
    let
        aFilePath = "some/File/Path.pa"

        run parser sourceCode =
            Parser.run aFilePath sourceCode parser
    in
    describe "Indentation Parser" <| do
        describe "topLevel" <| do
            it "Fails if not on column 1" <|
                let
                    source =
                        "if"

                    parser = do
                        _ <- C.anyChar
                        Indentation.topLevel

                    expected =
                        E.TopLevelIndentationExpected (Position aFilePath 1 2)
                            |> Left
                in
                run parser source `shouldBe` expected


            it "Succeeds even if there is spaces before next thing to parse" <|
                let
                    source =
                        " if"

                    parser =
                        Indentation.topLevel

                    expected =
                        Right ()
                in
                run parser source `shouldBe` expected


            it "Succeeds given the next thing to parse is on column 1" <|
                let
                    source =
                        "aFunction : Int -> Int"

                    parser =
                        Indentation.topLevel

                    expected =
                        Right ()
                in
                run parser source `shouldBe` expected


        describe "sameLine" <|
            let
                parser = do
                    _ <- C.string
                    C.someSpace
                    Indentation.sameLine
            in do
            it "Fails if not on same line than the position of reference" <|
                let
                    source =
                        [ "blah"
                        , "Bleuh"
                        ]
                            |> String.mergeLines

                    expected =
                        E.SameLineExpected (Position aFilePath 2 1)
                            |> Left
                in
                run parser source `shouldBe` expected


            it "Succeeds even if the next thing to parse is invalid given some spaces to parse first" <|
                let
                    source =
                        [ "blah"
                        , "Bleuh"
                        ]
                            |> String.mergeLines

                    expected =
                        E.SameLineExpected (Position aFilePath 2 1)
                            |> Left
                in
                run parser source `shouldBe` expected


            it "Succeeds given still on same line than the position of reference" <|
                let
                    source =
                        "blah Bleuh"

                    expected =
                        Right ()
                in
                run parser source `shouldBe` expected



        describe "sameLineOrIndented" <|
            let
                parser = do
                    _ <- C.string
                    C.someSpace
                    Indentation.sameLineOrIndented
            in do
            it "Fails if not on same line nor indented more than the position of reference" <|
                let
                    source =
                        [ "blah"
                        , "Bleuh"
                        ]
                            |> String.mergeLines

                    expected =
                        E.SameLineOrIndentedExpected (Position aFilePath 2 1)
                            |> Left
                in
                run parser source `shouldBe` expected


            it "Succeeds given on the same line than the position of reference" <|
                let
                    source =
                        "blah        Bleuh"

                    expected =
                        Right ()
                in
                run parser source `shouldBe` expected


            it "Succeeds given on another line but indented compared to the position of reference" <|
                let
                    source =
                        [ "blah"
                        , ""
                        , " Bleuh"
                        ]
                            |> String.mergeLines

                    expected =
                        Right ()
                in
                run parser source `shouldBe` expected


        describe "withPositionReference" <| do
            it "Fails given the new reference position messes up the indentation" <|
                let
                    source =
                        [ "bla"
                        , "    bleh"
                        , "    Bluh"
                        ]
                            |> String.mergeLines

                    parser = do
                        _ <- C.string
                        C.someSpace
                        Indentation.withPositionReference <| do
                            _ <- C.string
                            C.someSpace
                            Indentation.sameLineOrIndented
                            C.string


                    expected =
                        E.SameLineOrIndentedExpected (Position aFilePath 3 5)
                            |> Left
                in
                run parser source `shouldBe` expected


            it "Restores the reference position once the given parser is executed" <|
                let
                    source =
                        [ "bla"
                        , "    bleh"
                        , "    Bluh"
                        ]
                            |> String.mergeLines

                    parser = do
                        _ <- C.string
                        C.someSpace
                        _ <- Indentation.withPositionReference C.string
                        C.someSpace
                        Indentation.sameLineOrIndented
                        _ <- C.string
                        return ()


                    expected =
                        Right ()
                in
                run parser source `shouldBe` expected
