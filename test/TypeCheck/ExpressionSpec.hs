module TypeCheck.ExpressionSpec where

import Test.Hspec hiding (context)

import qualified Data.List.NonEmpty as NonEmpty

import qualified AST.Expression as E
import qualified Type as T
import qualified TypeCheck.Check as Check
import TypeCheck.Expression (checkExpression)


spec :: Spec
spec = do
    describe "Check" <|
        describe "Lambda" <| do
            it "returns the type for a (a -> a) lambda" <| do
                let params = NonEmpty.fromList [ "a" ]
                let body = E.Reference "a"
                let actual =
                        E.Lambda params body
                            |> checkExpression
                            |> Check.run
                let expected =
                        T.FunctionType (T.Variable 0) (T.Variable 0)
                            |> T.Function
                actual `shouldBe` (Right expected)

            it "returns the type for a (a -> Int) lambda" <| do
                let params = NonEmpty.fromList [ "a" ]
                let body = E.Value <| E.Int 3
                let actual =
                        E.Lambda params body
                            |> checkExpression
                            |> Check.run
                let expected =
                        T.FunctionType (T.Variable 0) T.Int
                            |> T.Function
                actual `shouldBe` (Right expected)

            it "returns the type for a lambda that has one constrained type" <| do
                let context =
                        ("add"
                        , T.FunctionType T.Float T.Float
                            |> T.Function
                            |> T.FunctionType T.Float
                            |> T.Function
                        )
                            |>
                                (\v ->
                                    Check.initialContext
                                        { Check.variables = [v] }
                                )
                let params = NonEmpty.fromList [ "a", "b" ]

                let applicationArgs =
                       NonEmpty.fromList
                            [ E.ReferenceArgument "b"
                            , E.ValueArgument <| E.Float 4.3
                            ]
                let body = E.Application "add" applicationArgs

                let actual =
                        E.Lambda params body
                            |> checkExpression
                            |> Check.runWithContext context
                let expected =
                        T.FunctionType T.Float T.Float
                                |> T.Function
                                |> T.FunctionType (T.Variable 0)
                                |> T.Function
                actual `shouldBe` (Right expected)

