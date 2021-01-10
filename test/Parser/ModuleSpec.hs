module Parser.ModuleSpec where

import Test.Hspec hiding (context)

import Control.Monad (fail)

import AST.CodeQuote (CodeQuote(..), Position(..))
import qualified AST.Identifier as Identifier
import qualified AST.Module as M
import qualified AST.TypeAnnotation as TA
import Parser.Module
import qualified Parser.Parser as Parser
import Printer.Utils (indent)
import qualified Utils.NonEmpty as NonEmpty
import qualified Utils.List as List
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
                    [ "type alias Foo a b c ="
                    , indent "{ a : a"
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
                        [ Identifier.typeVariableIdForTest "a"
                        , Identifier.typeVariableIdForTest "b"
                        , Identifier.typeVariableIdForTest "c"
                        ]
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
                                (buildCodeQuote 2 7 2 11)
                                (Identifier.dataIdForTest "a")
                                (Identifier.typeVariableIdForTest "a"
                                    |> TA.Variable
                                )
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


    describe "Given an invalid record" <|
        let
            parse =
                String.mergeLines
                    >> Parser.runParser moduleParser aFilePath

            expectFailure fileContent expectedError =
                case parse fileContent of
                    Right _ ->
                        "This was expected to fail with "
                            ++ show expectedError
                            |> fail

                    Left e ->
                        e `shouldBe` [expectedError]
        in do
        -- it "fails given a missing type" <|
        --     let
        --         fileContent =
        --             [ "type alias Foo ="
        --             , indent "{ a : "
        --             , indent ", b : Int"
        --             , indent "}"
        --             ]

        --         expected =
        --             Position aFilePath 2 7
        --                 |> Parser.FieldInvalid Parser.MustHaveTypeAnnotation
        --     in do
        --     expectFailure fileContent expected


        it "fails given a '=' instead of a ':'" <|
            let
                fileContent =
                    [ "type alias Foo ="
                    , indent "{ a : Bool "
                    , indent ", b = Int"
                    , indent "}"
                    ]

                expected =
                    Position aFilePath 3 7
                        |> Parser.FieldInvalid Parser.DefinitionMustUseColon
            in do
            expectFailure fileContent expected


        -- it "fails given a double coma" <|
        --     let
        --         fileContent =
        --             [ "type alias Foo ="
        --             , indent "{ a : Bool,"
        --             , indent ", b : Int"
        --             , indent "}"
        --             ]

        --         expected =
        --             Position aFilePath 2 15
        --                 |> Parser.RecordInvalid Parser.ExtraComma
        --                 |> List.singleton
        --     in do
        --     expectFailure fileContent expected

        -- it "fails given a trailing coma" <|
        --     let
        --         fileContent =
        --             [ "type alias Foo ="
        --             , indent "{ a : Bool"
        --             , indent ", b : Int"
        --             , indent ","
        --             , indent "}"
        --             ]

        --         expected =
        --             Position aFilePath 4 5
        --                 |> Parser.RecordInvalid Parser.ExtraComma
        --                 |> List.singleton
        --     in do
        --     expectFailure fileContent expected


        -- it "fails given trailing characters" <|
        --     let
        --         fileContent =
        --             [ "type alias Foo ="
        --             , indent "{ a : Bool"
        --             , indent ", b : Int"
        --             , indent "}}"
        --             ]

        --         expected =
        --             Position aFilePath 4 6
        --                 |> Parser.RecordInvalid Parser.TrailingCharacter
        --                 |> List.singleton
        --     in do
        --     expectFailure fileContent expected


        -- it "fails given an invalid alias name" <|
        --     let
        --         fileContent =
        --             [ "type alias foo ="
        --             , indent "{ a : Bool }"
        --             ]

        --         expected =
        --             Position aFilePath 1 12
        --                 |> Parser.TypeAliasInvalid Parser.TypeNameInvalid
        --                 |> List.singleton
        --     in do
        --     expectFailure fileContent expected


        -- it "fails given an invalid type variable in signature" <|
        --     let
        --         fileContent =
        --             [ "type alias Foo A ="
        --             , indent "{ a : Bool }"
        --             ]

        --         expected =
        --             Position aFilePath 1 12
        --                 |> Parser.TypeAliasInvalid Parser.TypeNameInvalid
        --                 |> List.singleton
        --     in do
        --     expectFailure fileContent expected
