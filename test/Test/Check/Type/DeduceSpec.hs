module Test.Check.Type.DeduceSpec where

import Test.Hspec hiding (context)

import Test.Parser.Sample (aQuote)

import AST.Identifier (DataId(..), ReferenceId(..))
import qualified Check.Type.Arrange as A
import qualified Check.Type.Deduce as D
import qualified Check.Type.Model as T
import qualified Utils.Map as Map


spec :: Spec
spec =
    fdescribe "Deduce" <|
        let
            aLink =
                A.Link 2
        in do
        it "Adds the deduction given an int expression" <|
            let
                expression =
                    A.Value
                        { link = aLink
                        , value = A.Int aQuote 3
                        }

                actual =
                    D.deduceType [expression]

                expected =
                    [ ( aLink, T.Int ) ]
                        |> Map.fromList
                        |> Right
            in
            actual `shouldBe` expected


        it "Adds the deduction given a float expression" <|
            let
                expression =
                    A.Value
                        { link = aLink
                        , value = A.Float aQuote 3
                        }

                actual =
                    D.deduceType [expression]

                expected =
                    [ ( aLink, T.Float ) ]
                        |> Map.fromList
                        |> Right
            in
            actual `shouldBe` expected


        it "Adds the deduction given a context reference" <|
            let
                expression =
                    A.ContextReference
                        { link = aLink
                        , type_ = T.Float
                        }

                actual =
                    D.deduceType [expression]

                expected =
                    [ ( aLink, A.type_ expression ) ]
                        |> Map.fromList
                        |> Right
            in
            actual `shouldBe` expected
