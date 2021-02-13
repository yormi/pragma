module Parser3.Expression.LambdaSpec where

import Test.Hspec hiding (context)

import qualified Parser3.Utils as Utils

import qualified AST3.Expression as Expression
import qualified Parser3.Expression as Expression
import qualified Utils.NonEmpty as NonEmpty
import qualified Utils.String as String


spec :: Spec
spec =
    let
        run sourceCode =
            Utils.run sourceCode Expression.expressionParser
    in
    describe "Lambda Parser" <| do
        it "Parses a lambda with spaces before" <|
            let
                source =
                    "  \\x -> True"

                expected arg =
                    Expression.Lambda
                        (Utils.position 1 3)
                        (NonEmpty.singleton arg)
                        (Utils.true 1 9)
                        |> Right
            in do
            arg <- Utils.dataId 1 4 "x"
            run source `shouldBe` expected arg


        it "Parses a single-line lambda" <|
            let
                source =
                    "\\x -> True"

                expected arg =
                    Expression.Lambda
                        (Utils.position 1 1)
                        (NonEmpty.singleton arg)
                        (Utils.true 1 7)
                        |> Right
            in do
            arg <- Utils.dataId 1 2 "x"
            run source `shouldBe` expected arg


        it "Parses a multi-line lambda" <|
            let
                source =
                    [ "\\x ->"
                    , "    3"
                    ]
                        |> String.mergeLines

                expected arg =
                    Expression.Lambda
                        (Utils.position 1 1)
                        (NonEmpty.singleton arg)
                        (Utils.int 2 5 3)
                        |> Right
            in do
            arg <- Utils.dataId 1 2 "x"
            run source `shouldBe` expected arg


        it "Parses a multi arguments lambda" <|
            let
                source =
                    [ "\\x y ->"
                    , "    3"
                    ]
                        |> String.mergeLines

                expected arg1 arg2 =
                    Expression.Lambda
                        (Utils.position 1 1)
                        (NonEmpty.build arg1 [arg2])
                        (Utils.int 2 5 3)
                        |> Right
            in do
            arg1 <- Utils.dataId 1 2 "x"
            arg2 <- Utils.dataId 1 4 "y"
            run source `shouldBe` expected arg1 arg2
