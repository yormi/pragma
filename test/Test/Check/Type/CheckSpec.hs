module Test.Check.Type.CheckSpec where

import Test.Hspec hiding (context)

import Test.Check.Type.Utils (aLink)
import Test.Parser.Sample (aQuote)

import qualified AST.TypeAnnotation as TA
import qualified Check.Type.Constraint as C
import Check.Type.Check
import qualified Check.Type.Model.PrimitiveType as Primitive
import qualified Check.Type.Check.Model.InstancedType as I
import qualified Check.Type.Check.Model.Error as Error
import qualified Check.Type.Check.Model.Solution as Solution


spec :: Spec
spec =
    fdescribe "Check" <| do
        it "Returns Nothing given no constraints" <|
            check [] `shouldBe` Nothing


        it "Returns Nothing given a primitive constraint" <|
            let
                constraints =
                    [ C.Primitive aLink aQuote Primitive.Int ]

                expected =
                    Nothing
            in
            check constraints `shouldBe` expected


        describe "Future" <| do
            it "Returns Nothing given the link is already resolved" <|
                let
                    constraints =
                        [ C.Primitive aLink aQuote Primitive.Int
                        , C.Future aLink
                        ]

                    expected =
                        Nothing
                in
                check constraints `shouldBe` expected


            it "Errors with ThisIsABug given the link is already resolved" <|
                let
                    constraints =
                        [ C.Future aLink
                        ]

                    expected result =
                        case result of
                            Just (Error.ThisIsABug _) ->
                                True

                            _ ->
                                False
                in
                check constraints `shouldSatisfy` expected


        describe "IfCondition" <|
            let
                constraint =
                    C.IfCondition aLink
            in do
            it "Returns Nothing given the link resolve to a Bool" <|
                let
                    constraints =
                        [ C.Primitive aLink aQuote Primitive.Bool
                        , constraint
                        ]

                    expected =
                        Nothing
                in
                check constraints `shouldBe` expected


            it "Returns Nothing given the link resolve to a Bool" <|
                let
                    constraints =
                        [ C.Primitive aLink aQuote Primitive.Bool
                        , constraint
                        ]

                    expected =
                        Nothing
                in
                check constraints `shouldBe` expected


            it "Errors with ThisIsABug given the link does not resolve" <|
                let
                    constraints =
                        [ C.Future aLink
                        , constraint
                        ]

                    expected result =
                        case result of
                            Just (Error.ThisIsABug _) ->
                                True

                            _ ->
                                False
                in
                check constraints `shouldSatisfy` expected


            it "Errors with IfConditionMustBeABool given the link does not resolve" <|
                let
                    constraints =
                        [ C.Primitive aLink aQuote Primitive.String
                        , constraint
                        ]

                    expected =
                        Solution.Instanced I.String
                            |> Error.IfConditionMustBeABool
                            |> Just
                in
                check constraints `shouldBe` expected


        describe "ContextReference" <|
            let
                constraint =
                     C.ContextReference aLink TA.Int
            in do
            it "Returns Nothing given reference type annotation is completely bound" <|
                let
                    constraints =
                        [ constraint ]

                    expected =
                        Nothing
                in
                check constraints `shouldBe` expected
