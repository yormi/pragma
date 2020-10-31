module Type.Constraint.ExpressionSpec where

import Test.Hspec hiding (context)

import Control.Monad (fail)
--import qualified Data.List.NonEmpty as NonEmpty

import qualified AST.Expression as E
import qualified Type as T
import qualified Type.Constraint.Gatherer as Gatherer
import qualified Type.Constraint.Expression as Expression


spec :: Spec
spec =
    let
        gather =
                Expression.gather >> Gatherer.gatherConstraints

        eval =
                Expression.gather >> Gatherer.eval

        expectRight expectation actual =
            case actual of
                Right cs ->
                    expectation cs

                Left e ->
                    fail <| "Constraint Gathering failed with : " ++ show e
    in do
    describe "Gather Constraint" <| do
        describe "Value" <|
            let
                testNoConstraint =
                    E.Value
                        >> gather
                        >> shouldBe (Right [])

                evalValue =
                    E.Value >> eval

                test name ast expectedType =
                    describe name <| do
                        it "returns no constraint" <|
                            testNoConstraint ast

                        it ("returns type " ++ show expectedType) <|
                            evalValue ast `shouldBe` (Right expectedType)
            in do
            test "Bool" (E.Bool E.TrueLiteral) T.Bool
            test "Char" (E.Char 'e') T.Char
            test "Float" (E.Float 3.14) T.Float
            test "Int" (E.Int 3) T.Int
            test "String" (E.String "banana") T.String


        describe "Reference" <|
            let
                anIdentifier =
                    "x"

                ast =
                    E.Reference anIdentifier
            in do
            it "fails given there is no matching identifier in the environment"
                (Expression.gather ast
                    |> Gatherer.withEnv []
                    |> Gatherer.eval
                    |> shouldBe (Left <| Gatherer.UnboundVariable anIdentifier)
                )

            it "returns the type variable it is referencing to, given one" <|
                let
                    typeVariable =
                        3
                in
                Expression.gather ast
                    |> Gatherer.withEnv [ (anIdentifier, typeVariable) ]
                    |> Gatherer.eval
                    |> shouldBe (Right <| T.Variable typeVariable)


        describe "If-Then-Else" <| do
            it "returns the type of the true alternative expression" <|
                let
                    ast =
                        E.If
                            { E.condition =
                                E.Value <| E.Bool E.FalseLiteral
                            , E.whenTrue = E.Value <| E.String "true"
                            , E.whenFalse = E.Value <| E.String "false"
                            }
                in
                Expression.gather ast
                    |> Gatherer.eval
                    |> shouldBe (Right T.String )


            it "adds a constraint between Bool and the condition expression" <|
                let
                    ast =
                        E.If
                            { E.condition =
                                E.Value <| E.String "not right but anyway"
                            , E.whenTrue = E.Value <| E.String "true"
                            , E.whenFalse = E.Value <| E.String "false"
                            }

                in
                gather ast
                    |> expectRight (`shouldContain` [ (T.Bool, T.String) ])


            it "adds a constraint between both alternative expressions" <|
                let
                    ast =
                        E.If
                            { E.condition =
                                E.Value <| E.String "not right but anyway"
                            , E.whenTrue = E.Value <| E.Int 3
                            , E.whenFalse = E.Value <| E.Float 2.2
                            }
                in
                gather ast
                    |> expectRight (`shouldContain` [ (T.Int, T.Float) ])


        --describe "Lambda" <| do
        --    it "returns the type for a (a -> a) lambda" <| do
        --        let params = NonEmpty.fromList [ "a" ]
        --        let body = E.Reference "a"
        --        let actual =
        --                E.Lambda params body
        --                    |> checkExpression
        --                    |> Check.run
        --        let expected =
        --                T.FunctionType (T.Variable 0) (T.Variable 0)
        --                    |> T.Function
        --        actual `shouldBe` (Right expected)

        --    it "returns the type for a (a -> Int) lambda" <| do
        --        let params = NonEmpty.fromList [ "a" ]
        --        let body = E.Value <| E.Int 3
        --        let actual =
        --                E.Lambda params body
        --                    |> checkExpression
        --                    |> Check.run
        --        let expected =
        --                T.FunctionType (T.Variable 0) T.Int
        --                    |> T.Function
        --        actual `shouldBe` (Right expected)

        --    it "returns the type for a lambda that has one constrained type" <| do
        --        let context =
        --                ("add"
        --                , T.FunctionType T.Float T.Float
        --                    |> T.Function
        --                    |> T.FunctionType T.Float
        --                    |> T.Function
        --                )
        --                    |>
        --                        (\v ->
        --                            Check.initialContext
        --                                { Check.variables = [v] }
        --                        )
        --        let params = NonEmpty.fromList [ "a", "b" ]

        --        let applicationArgs =
        --               NonEmpty.fromList
        --                    [ E.ReferenceArgument "b"
        --                    , E.ValueArgument <| E.Float 4.3
        --                    ]
        --        let body = E.Application "add" applicationArgs

        --        let actual =
        --                E.Lambda params body
        --                    |> checkExpression
        --                    |> Check.runWithContext context
        --        let expected =
        --                T.FunctionType T.Float T.Float
        --                        |> T.Function
        --                        |> T.FunctionType (T.Variable 0)
        --                        |> T.Function
        --        actual `shouldBe` (Right expected)

