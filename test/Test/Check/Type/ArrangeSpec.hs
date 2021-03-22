module Test.Check.Type.ArrangeSpec where

import Test.Hspec hiding (context)

import Test.Parser.Sample (aQuote)

import qualified Check.Type.Arrange as A
import qualified Check.Type.Futurize as F
import qualified Check.Type.Model.PrimitiveType as Primitive
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
                                F.Primitive aQuote Primitive.Int
                                    |> F.Definition "x" (F.Placeholder 0)
                                    |> NonEmpty.singleton
                            , body =
                                F.Future <| F.Placeholder 0
                            }

                    expected =
                        [ A.Primitive (A.Link 0) aQuote Primitive.Int
                        , A.Definition (A.Link 0)
                        , A.Future (A.Link 0)
                        ]
                            |> Right
                in
                A.arrange expression `shouldBe` expected


            describe "Arranges the let definitions in a dependancy order. The one with no dependance first and then the dependant one" <|
                let
                    dependantDefinition =
                        F.Placeholder 0
                            |> F.Future
                            |> F.Definition "y" (F.Placeholder 1)

                    independantDefinition =
                        F.Primitive aQuote Primitive.Int
                            |> F.Definition "x" (F.Placeholder 0)

                    body =
                        F.Future <| F.Placeholder 1

                    expected =
                        [ A.Primitive (A.Link 0) aQuote Primitive.Int
                        , A.Definition (A.Link 0)
                        , A.Future (A.Link 0)
                        , A.Definition (A.Link 0)
                        , A.Future (A.Link 0)
                        ]
                            |> Right
                in do
                it "First definition depends on the second one" <|
                    let
                        expression =
                            F.LetIn
                                { definitions =
                                    NonEmpty.build
                                        dependantDefinition
                                        [ independantDefinition ]
                                , body = body
                                }
                    in
                    A.arrange expression `shouldBe` expected


                it "Second definition depends on the first one" <|
                    let
                        expression =
                            F.LetIn
                                { definitions =
                                    NonEmpty.build
                                        independantDefinition
                                        [ dependantDefinition ]
                                , body = body
                                }
                    in
                    A.arrange expression `shouldBe` expected


            it "fails on a definition dependency cycle" <|
                let
                    dependantDefinition1 =
                        F.Placeholder 0
                            |> F.Future
                            |> F.Definition "x" (F.Placeholder 1)

                    dependantDefinition2 =
                        F.Placeholder 1
                            |> F.Future
                            |> F.Definition "y" (F.Placeholder 0)

                    expression =
                        F.LetIn
                            { definitions =
                                NonEmpty.build
                                    dependantDefinition1
                                    [ dependantDefinition2 ]
                            , body = F.Future <| F.Placeholder 1
                            }

                    expected =
                        Left A.CycleInLetDefinitionDependencies
                in
                A.arrange expression `shouldBe` expected
