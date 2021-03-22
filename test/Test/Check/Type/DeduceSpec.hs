module Test.Check.Type.DeduceSpec where

import Test.Hspec hiding (context)

import Test.Parser.Sample (aQuote)

import qualified AST.TypeAnnotation as TA
import qualified Check.Type.Arrange as A
import qualified Check.Type.Deduce as D
import qualified Check.Type.Model as T
import qualified Check.Type.Model.PrimitiveType as Primitive
import qualified Utils.Map as Map


spec :: Spec
spec =
    describe "Deduce" <|
        let
            aLink =
                A.Link 2
        in do
        describe "Primitive" <| do
            it "Adds the deduction given an int expression" <|
                let
                    expression =
                        A.Primitive
                            { link = aLink
                            , quote = aQuote
                            , primitiveType = Primitive.Int
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
                        A.Primitive
                            { link = aLink
                            , quote = aQuote
                            , primitiveType = Primitive.Float
                            }

                    actual =
                        D.deduceType [expression]

                    expected =
                        [ ( aLink, T.Float ) ]
                            |> Map.fromList
                            |> Right
                in
                actual `shouldBe` expected


        describe "ContextReference" <| do
            it "Adds the deduction given a context reference" <|
                let
                    expression =
                        A.ContextReference
                            { link = aLink
                            , annotation = TA.Float
                            }

                    actual =
                        D.deduceType [expression]

                    expected =
                        [ ( aLink, T.Float ) ]
                            |> Map.fromList
                            |> Right
                in
                actual `shouldBe` expected
