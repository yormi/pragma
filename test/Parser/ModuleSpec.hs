module Parser.ModuleSpec where

import Test.Hspec hiding (context)

import Control.Monad (fail)

import AST.CodeQuote (CodeQuote(..))
import qualified AST.Identifier as Identifier
import qualified AST.Module as M
import qualified AST.TypeAnnotation as TA
import Parser.Module
import qualified Parser.Parser as Parser
import Printer.Utils (indent)
import qualified Utils.NonEmpty as NonEmpty
import qualified Utils.String as String


spec :: Spec
spec =
    let
        aFilePath =
            "someFilePath"
    in do
    describe "Record" <| do
        describe "Given a valid record definition" <|
            let
                fileContent =
                    [ "type alias Foo ="
                    , indent "{ a : Int"
                    , indent ", b : Bool"
                    , indent ", c : Char"
                    , indent "}"
                    ]
                        |> String.mergeLines

                parsed =
                    Parser.runParser moduleParser aFilePath fileContent

                buildCodeQuote fromLine fromColumn toLine toColumn =
                    CodeQuote
                        { filename = aFilePath
                        , fromLine = fromLine
                        , fromColumn = fromColumn
                        , toLine = toLine
                        , toColumn = toColumn
                        }

                expect expectation =
                    case parsed of
                        Right (M.Module (record@(M.Record {}) : [])) ->
                            expectation record

                        _ ->
                            "The parsing is not a unique record but is: "
                                ++ show parsed
                                |> fail
            in do
            it "parses the right code quote" <|
                let

                    expected =
                        buildCodeQuote 1 1 5 5
                in do
                expect
                    (\M.Record { codeQuote } ->
                        codeQuote `shouldBe` expected
                    )


            it "parses the right typeName" <|
                let
                    expected =
                        Identifier.typeIdForTest "Foo"
                in do
                expect
                    (\M.Record { typeName } ->
                        typeName `shouldBe` expected
                    )


            it "parses the right typeVariables" <|
                let
                    expected =
                            []
                in do
                expect
                    (\M.Record { typeVariables } ->
                        typeVariables `shouldBe` expected
                    )


            it "parses the right fields" <|
                let
                    expected =
                        NonEmpty.build
                            (M.Field
                                (buildCodeQuote 2 7 2 13)
                                (Identifier.dataIdForTest "a")
                                TA.Int
                            )
                            [ M.Field
                                (buildCodeQuote 3 7 3 14)
                                (Identifier.dataIdForTest "b")
                                TA.Bool
                            , M.Field
                                (buildCodeQuote 4 7 4 14)
                                (Identifier.dataIdForTest "c")
                                TA.Char
                            ]
                in do
                expect
                    (\M.Record { fields } ->
                        fields `shouldBe` expected
                    )
