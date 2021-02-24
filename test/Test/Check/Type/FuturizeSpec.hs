module Test.Check.Type.FuturizeSpec where

import Test.Hspec hiding (context)

import qualified Data.Map as Map

import Test.Parser.Sample (aQuote)

import AST.Identifier (DataId(..), ReferenceId(..))
import qualified Check.Type.Futurize as F
import qualified Check.Type.ReplaceTopLevel as R
import qualified Check.Type.Model as Type
import qualified Utils.NonEmpty as NonEmpty


spec :: Spec
spec =
    describe "Futurize" <| do
        it "In the body, replaces the reference defined in the LetIn with a placeholder" <|
            let
                expression =
                    R.LetIn
                        { definitions =
                            R.Int aQuote 3
                                |> R.Value
                                |> R.SimpleDefinition (DataId aQuote "x")
                                |> NonEmpty.singleton
                        , body = R.Reference (ReferenceId aQuote "x")
                        }

                expected =
                    F.LetIn
                        { definitions =
                            F.Int aQuote 3
                                |> F.Value
                                |> F.Definition "x" (F.Placeholder 0)
                                |> NonEmpty.singleton
                        , body =
                            F.Future <| F.Placeholder 0
                        }
                        |> Right
            in
            F.futurize expression `shouldBe` expected


        it "In the same let, in another definition, replaces the reference defined in the same LetIn with a placeholder" <|
            let
                expression =
                    R.LetIn
                        { definitions =
                            NonEmpty.build
                                (R.Int aQuote 3
                                    |> R.Value
                                    |> R.SimpleDefinition (DataId aQuote "x")
                                )
                                [R.Reference (ReferenceId aQuote "x")
                                    |> R.SimpleDefinition (DataId aQuote "y")
                                ]
                        , body = R.Reference (ReferenceId aQuote "y")
                        }

                expected =
                    F.LetIn
                        { definitions =
                            NonEmpty.build
                                (F.Int aQuote 3
                                    |> F.Value
                                    |> F.Definition "x" (F.Placeholder 0)
                                )
                                [F.Future (F.Placeholder 0)
                                    |> F.Definition "y" (F.Placeholder 1)
                                ]
                        , body =
                            F.Future <| F.Placeholder 1
                        }
                        |> Right
            in
            F.futurize expression `shouldBe` expected
