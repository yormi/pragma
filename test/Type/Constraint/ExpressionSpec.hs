module Type.Constraint.ExpressionSpec where

import Test.Hspec hiding (context)

import Control.Monad (fail)
import qualified Data.List.NonEmpty as NonEmpty

import qualified AST.Expression as E
import qualified Type.Model as T
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
                    aTypeVariable =
                        T.Variable 3
                in
                Expression.gather ast
                    |> Gatherer.withEnv [ (anIdentifier, aTypeVariable) ]
                    |> Gatherer.eval
                    |> shouldBe (Right <| aTypeVariable)


        describe "If Then Else" <| do
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


        describe "Let In" <| do
            it "returns the type of the body" <|
                let
                    ast =
                        E.LetIn
                            { E.definitions =
                                NonEmpty.fromList
                                    [ E.SimpleDefinition "a"
                                        (E.Value <| E.String "aa")
                                    , E.SimpleDefinition "b"
                                        (E.Value <| E.String "bb")
                                    ]
                            , E.body = E.Value <| E.Int 3
                            }
                in
                eval ast `shouldBe` (Right T.Int)


            it "adds the definitions to the environment of the body" <|
                let
                    ast reference =
                        E.LetIn
                            { E.definitions =
                                NonEmpty.fromList
                                    [ E.SimpleDefinition "a"
                                        (E.Value <| E.Int 3)
                                    , E.SimpleDefinition "b"
                                        (E.Value <| E.Float 3.14)
                                    ]
                            , E.body = E.Reference reference
                            }
                in do
                ast "a"
                    |> eval
                    |> expectRight (`shouldBe` T.Int)

                ast "b"
                    |> eval
                    |> expectRight (`shouldBe` T.Float)





        describe "Lambda" <| do
            describe "a -> a" <|
                let
                    params =
                        NonEmpty.fromList [ "a" ]

                    body =
                        E.Reference "a"

                    actual =
                        eval <| E.Lambda params body
                in do
                it "returns the (a -> a) type" <|
                    let
                        expected =
                            T.FunctionType (T.Variable 0) (T.Variable 0)
                                |> T.Function
                    in
                    actual `shouldBe` (Right expected)


            it "returns the type for a (a -> Int) lambda" <|
                let
                    params =
                        NonEmpty.fromList [ "a" ]

                    body =
                        E.Value <| E.Int 3

                    actual =
                        eval <| E.Lambda params body

                    expected =
                        T.FunctionType (T.Variable 0) T.Int
                            |> T.Function
                in
                actual `shouldBe` (Right expected)


            describe "Int -> Int lambda resolved because of constraint from the body" <|
                let
                    env =
                        T.FunctionType T.Int T.Float
                            |> T.Function
                            |> (\t -> [ ("aIntToFloatFunction", t) ])

                    params =
                        NonEmpty.fromList [ "a" ]

                    body =
                        E.Application
                            { E.functionName = "aIntToFloatFunction"
                            , E.args =
                                NonEmpty.fromList [E.Reference "a"]
                            }

                    gatherer =
                        E.Lambda params body
                            |> Expression.gather
                            |> Gatherer.withEnv env
                in do
                it "returns the type for a (a -> Int) lambda" <|
                    let
                        actual =
                            Gatherer.eval gatherer

                        expected =
                            T.FunctionType (T.Variable 0) T.Float
                                |> T.Function
                    in
                    actual `shouldBe` (Right expected)


                it "adds a constraint between the argument and the applied function param" <|
                    let
                        actual =
                            Gatherer.gatherConstraints gatherer

                        expected =
                            [(T.Variable 0, T.Int)]
                    in
                    actual `shouldBe` (Right expected)

        describe "Application" <|
            let
                functionType =
                    T.FunctionType T.Float T.String
                        |> T.Function
                        |> T.FunctionType T.Int
                        |> T.Function

                env =
                    [ ("aIntToFloatToString", functionType) ]

                gatherer args =
                    E.Application
                        { E.functionName = "aIntToFloatToString"
                        , E.args = args
                        }
                        |> Expression.gather
                        |> Gatherer.withEnv env
            in do
            describe "Partial Application" <|
                let
                    arguments =
                        NonEmpty.fromList [E.Value <| E.Int 3]
                in do
                it "returns the type returned by the partial application" <|
                    let
                        actual =
                            Gatherer.eval <| gatherer arguments

                        expected =
                            T.FunctionType T.Float T.String
                                |> T.Function
                    in
                    actual `shouldBe` (Right expected)


                it "adds a constraint between the argument and the param" <|
                    let
                        actual =
                            Gatherer.gatherConstraints <| gatherer arguments

                        expected =
                            [(T.Int, T.Int)]
                    in
                    actual `shouldBe` (Right expected)


            describe "Full Application" <|
                let
                    aReference =
                        E.Reference "x"

                    fullGatherer =
                        NonEmpty.fromList
                            [ E.Value <| E.Int 3
                            , aReference
                            ]
                            |> gatherer
                            |> Gatherer.withEnv [("x", T.Variable 0)]
                in do
                it "returns the type returned by the application of 2 args" <|
                    Gatherer.eval fullGatherer `shouldBe` (Right T.String)

                it "adds a constraint for all arguments" <|
                    let
                        actual =
                            Gatherer.gatherConstraints fullGatherer

                        expected =
                            [ (T.Int, T.Int)
                            , (T.Variable 0, T.Float)
                            ]
                    in
                    actual `shouldBe` (Right expected)

            describe "Too many arguments" <|
                it "fails with a TooManyArguments error" <|
                    let
                        arguments =
                            NonEmpty.fromList
                                [ E.Value <| E.Int 3
                                , E.Value <| E.Int 3
                                , E.Value <| E.Int 3
                                , E.Value <| E.Int 3
                                ]

                        actual =
                            arguments
                                |> gatherer
                                |> Gatherer.eval

                        expected =
                            Gatherer.TooManyArguments functionType arguments
                    in do
                    actual `shouldBe` (Left expected)
