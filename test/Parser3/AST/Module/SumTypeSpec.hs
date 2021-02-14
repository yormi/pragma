module Parser3.AST.Module.SumTypeSpec where

import Test.Hspec hiding (context)

import qualified Test.Utils as TestUtils
import qualified Parser3.Utils as Utils

import qualified AST3.Module as Module
import qualified AST3.TypeAnnotation as Annotation
import qualified Parser3.AST.Module as Module
import qualified Utils.List as List
import qualified Utils.NonEmpty as NonEmpty
import qualified Utils.OrderedSet as OrderedSet
import qualified Utils.String as String


spec :: Spec
spec =
    let
        noTypeVariables =
            OrderedSet.empty

        formatExpected typeVariables constructors = do
            typeId <- Utils.typeId 1 6 "Color"
            Module.SumType
                { fromPosition = Utils.position 1 1
                , typeName = typeId
                , typeVariables = typeVariables
                , dataChoices = constructors
                }
                |> List.singleton
                |> Module.Module
                |> Right
                |> return

        run sourceCode =
            Utils.run sourceCode Module.moduleParser
    in
    describe "Sum Type Parser" <| do
        it "Parses a sum type with one constructor" <|
            let
                source =
                    "type Color = Blue"

            in do
            constructorId <- Utils.constructorId 1 14 "Blue"
            let constructors =
                    Module.DataChoice
                        { tag = constructorId
                        , args = []
                        }
                        |> NonEmpty.singleton
            expected <- formatExpected noTypeVariables constructors
            run source `shouldBe` expected


        it "Parses a sum type with many constructors" <|
            let
                source =
                    [ "type Color"
                    , "    = Blue"
                    , "    | Red"
                    , "    | Green"
                    ]
                        |> String.mergeLines

            in do
            blue <- Utils.constructorId 2 7 "Blue"
            red <- Utils.constructorId 3 7 "Red"
            green <- Utils.constructorId 4 7 "Green"
            constructors <-
                [ blue, red, green ]
                    |> map
                        (\constructorId ->
                            Module.DataChoice
                                { tag = constructorId
                                , args = []
                                }
                        )
                    |> NonEmpty.fromList
                    |> TestUtils.assumeJust

            expected <- formatExpected noTypeVariables constructors
            run source `shouldBe` expected


        it "Parses a sum type with many constructors with constructor arguments" <|
            let
                source =
                    [ "type Color"
                    , "    = Blue Int (Maybe String)"
                    , "    | Red Int"
                    ]
                        |> String.mergeLines
            in do
            maybeId <- Utils.typeId 2 17 "Maybe"
            blue <- Utils.constructorId 2 7 "Blue"
            let blueChoice =
                    Module.DataChoice
                        { tag = blue
                        , args =
                            [ Annotation.Int
                            , Annotation.Custom maybeId [Annotation.String]
                            ]
                        }
            red <- Utils.constructorId 3 7 "Red"
            let redChoice =
                    Module.DataChoice
                        { tag = red
                        , args = [ Annotation.Int ]
                        }
            let constructors = NonEmpty.build blueChoice [redChoice]
            expected <- formatExpected noTypeVariables constructors
            run source `shouldBe` expected


        it "Parses a sum type with many constructors" <|
            let
                source =
                    [ "type Color"
                    , "    = Blue"
                    , "    | Red"
                    , "    | Green"
                    ]
                        |> String.mergeLines
            in do
            blue <- Utils.constructorId 2 7 "Blue"
            red <- Utils.constructorId 3 7 "Red"
            green <- Utils.constructorId 4 7 "Green"
            constructors <-
                [ blue, red, green ]
                    |> map
                        (\constructorId ->
                            Module.DataChoice
                                { tag = constructorId
                                , args = []
                                }
                        )
                    |> NonEmpty.fromList
                    |> TestUtils.assumeJust

            expected <- formatExpected noTypeVariables constructors
            run source `shouldBe` expected




        it "Parses a sum type with many constructors with type parameters" <|
            let
                source =
                    [ "type Color a b c"
                    , "    = Blue a b"
                    , "    | Red c"
                    ]
                        |> String.mergeLines
            in do
            blue <- Utils.constructorId 2 7 "Blue"
            a <- Utils.typeVariableId 2 12 "a"
            b <- Utils.typeVariableId 2 14 "b"
            let blueChoice =
                    Module.DataChoice
                        { tag = blue
                        , args =
                            [ Annotation.Variable a
                            , Annotation.Variable b
                            ]
                        }

            red <- Utils.constructorId 3 7 "Red"
            c <- Utils.typeVariableId 3 11 "c"
            let redChoice =
                    Module.DataChoice
                        { tag = red
                        , args = [ Annotation.Variable c ]
                        }

            let constructors = NonEmpty.build blueChoice [redChoice]

            a' <- Utils.typeVariableId 1 12 "a"
            b' <- Utils.typeVariableId 1 14 "b"
            c' <- Utils.typeVariableId 1 16 "c"
            let variableDeclaration = OrderedSet.fromList [a', b', c']

            expected <- formatExpected variableDeclaration constructors
            run source `shouldBe` expected


        it "Parses a sum type with many constructors no matter the indentation" <|
            let
                source =
                    [ "type Color"
                    , "= Blue"
                    , "    | Red"
                    , "        | Green"
                    ]
                        |> String.mergeLines
            in do
            blue <- Utils.constructorId 2 3 "Blue"
            red <- Utils.constructorId 3 7 "Red"
            green <- Utils.constructorId 4 11 "Green"
            constructors <-
                [ blue, red, green ]
                    |> map
                        (\constructorId ->
                            Module.DataChoice
                                { tag = constructorId
                                , args = []
                                }
                        )
                    |> NonEmpty.fromList
                    |> TestUtils.assumeJust

            expected <- formatExpected noTypeVariables constructors
            run source `shouldBe` expected
