module Test.Check.Type.ReplaceTopLevelSpec where

import Test.Hspec hiding (context)

import Test.Parser.Sample (aQuote)

import qualified AST.Expression as E
import qualified Check.Type.Model.PrimitiveType as Primitive
import qualified Check.Type.ReplaceTopLevel as R
import qualified Utils.Map as Map


spec :: Spec
spec =
    describe "Value" <|
        let
            context =
                Map.empty

            params =
                []

            replace =
                R.replace context params
        in do
        it "returns a Bool primitive given a True literal" <|
            let
                expression =
                    E.Value (E.Bool <| E.TrueLiteral aQuote)

                expected =
                    R.Primitive aQuote Primitive.Bool
            in
            replace expression `shouldBe` expected


        it "returns a Bool primitive given a False literal" <|
            let
                expression =
                    E.Value (E.Bool <| E.FalseLiteral aQuote)

                expected =
                    R.Primitive aQuote Primitive.Bool
            in
            replace expression `shouldBe` expected


        it "returns an Int primitive given a Int" <|
            let
                expression =
                    E.Value (E.Int aQuote 3)

                expected =
                    R.Primitive aQuote Primitive.Int
            in
            replace expression `shouldBe` expected


        it "returns a Float primitive given a Float" <|
            let
                expression =
                    E.Value (E.Float aQuote 3.3)

                expected =
                    R.Primitive aQuote Primitive.Float
            in
            replace expression `shouldBe` expected


        it "returns a Char primitive given a Char" <|
            let
                expression =
                    E.Value (E.Char aQuote 'c')

                expected =
                    R.Primitive aQuote Primitive.Char
            in
            replace expression `shouldBe` expected


        it "returns a String primitive given a String" <|
            let
                expression =
                    E.Value (E.String aQuote "hello world")

                expected =
                    R.Primitive aQuote Primitive.String
            in
            replace expression `shouldBe` expected
