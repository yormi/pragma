module Test.Check.Type.ArrangeSpec where

import Test.Hspec hiding (context)

import qualified Data.Map as Map

import Test.Parser.Sample (aQuote)

import AST.Identifier (DataId(..), ReferenceId(..))
import qualified Check.Type.Arrange as A
import qualified Check.Type.Futurize as F
import qualified Check.Type.Model as Type
import qualified Utils.NonEmpty as NonEmpty


spec :: Spec
spec =
    describe "Arrange" <| do
        describe "Let..In" <| do
            it "Arranges the let definition body first, then the definition and finally, the let body" <|
                let
                    expression =
                        F.LetIn
                            { definitions =
                                F.Int aQuote 3
                                    |> F.Value
                                    |> F.Definition "x" (F.Placeholder 0)
                                    |> NonEmpty.singleton
                            , body =
                                F.Future <| F.Placeholder 0
                            }

                    expected =
                        [ A.Value (A.LinkPlaceholder 0) <| A.Int aQuote 3
                        , A.Definition
                            (A.LinkPlaceholder 1)
                            (F.Placeholder 0)
                            (A.LinkPlaceholder 0)
                        , A.Future (A.LinkPlaceholder 2) (F.Placeholder 0)
                        ]
                in
                A.arrange expression `shouldBe` expected
