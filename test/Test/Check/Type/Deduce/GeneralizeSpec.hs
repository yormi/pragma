module Test.Check.Type.Deduce.GeneralizeSpec where

import Test.Hspec hiding (context)

import Test.Parser.Sample (aQuote)

import qualified AST.TypeAnnotation as TA
import qualified Check.Type.Arrange as A
import qualified Check.Type.Deduce.Deducer as D
import qualified Check.Type.Deduce.Generalize as Generalize
import qualified Check.Type.Deduce.Model.GenericType as G
import qualified Check.Type.Model.PrimitiveType as Primitive
import qualified Check.Type.Model.Type as T
import qualified Utils.Map as Map


spec :: Spec
spec =
    let
        generalize type_ =
            type_
                |> Generalize.generalize
                |> D.run
    in
    describe "Generalize" <| do
        it "generalizes a Bool to a Bool" <|
            let
                expected =
                    Right G.Bool
            in
            generalize T.Bool `shouldBe` expected
