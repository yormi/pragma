module Parser3.IndentationSpec where

import Test.Hspec hiding (context)

import qualified Parser3.Combinator as C
import qualified Parser3.Model.Error as E
import Parser3.Model.Position (Position(..))
import Parser3.Model.Quote (Quote(..))
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


            it "Fails if next thing to parse is NOT on column 1" <|
                let
                    source =
                        " if"

                    parser =
                        Indentation.topLevel

                    expected =
                        E.TopLevelIndentationExpected (Position aFilePath 1 2)
                            |> Left
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


        describe "sameLineOrIndented" <| do
            it "Fails if not on same line nor indented more than the position of reference" <|
                let
                    source =
                        [ "blah"
                        , "Bleuh"
                        ]
                            |> String.mergeLines

                    parser = do
                        _ <- C.string
                        Indentation.sameLineOrIndented

                    expected =
                        E.SameLineOrIndentedExpected (Position aFilePath 2 1)
                            |> Left
                in
                run parser source `shouldBe` expected


            it "Succeeds given on the same line than the position of reference" <|
                let
                    source =
                        "blah        Bleuh"

                    parser = do
                        _ <- C.string
                        Indentation.sameLineOrIndented

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

                    parser = do
                        _ <- C.string
                        Indentation.sameLineOrIndented

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
                        Indentation.withPositionReference <| do
                            Indentation.sameLineOrIndented
                            _ <- C.string
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
                        Indentation.withPositionReference C.string
                        Indentation.sameLineOrIndented
                        C.string
                        return ()


                    expected =
                        Right ()
                in
                run parser source `shouldBe` expected
