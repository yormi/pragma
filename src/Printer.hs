module Printer (printModule) where

import Data.Functor ((<&>))
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.String as String
import qualified Text.PrettyPrint as P

import Debug.Trace

import Expression
    ( Argument(..)
    , BoolLiteral(..)
    , Case(..)
    , Definition(..)
    , Expr(..)
    , Identifier
    , Pattern(..)
    , Value(..)
    )
import Module (Module(..), TopLevel(..))
import qualified Module
import Type (Type)
import qualified Type
import qualified Utils.Maybe as Maybe


type Indentation = Int


printModule :: Module -> String
printModule (Module functions) =
    functions
        |> map topLevel
        |> List.intersperse "\n\n"
        |> String.unlines


topLevel :: TopLevel -> String
topLevel t =
    case t of
        Function
            { Module.type_
            , Module.functionName
            , Module.params
            , Module.body
            } ->
            let
                typeLine =
                    type_
                        |> map printType
                        |> map (\t -> functionName ++ " : " ++ t ++ "\n")
                        |> Maybe.withDefault ""


            in
            typeLine
                ++ functionName ++ " "
                ++ printParams params
                ++ " =\n"
                ++ (indent 1 <| expression body)


printType :: Type -> String
printType type_ =
    case type_ of
        Type.Int ->
            "Int"

        Type.Float ->
            "Float"

        Type.Char ->
            "Char"

        Type.String ->
            "String"

        Type.Function t1 t2 ->
            let
                formattedT1 =
                    case t1 of
                        Type.Function a b ->
                            t1
                                |> printType
                                |> parenthesized
                        _ ->
                            printType t1
            in
            formattedT1 ++ " -> " ++ printType t2


indent :: Indentation -> String -> String
indent indentation =
    let
        fourSpaceIndent =
            "    "

        spaces =
            List.replicate indentation fourSpaceIndent
                |> String.unwords
    in
    String.lines
        >> map
            (\line ->
                if line == [] then
                    line
                else
                    spaces ++ line
            )
        >> List.intercalate "\n"


identifier :: Identifier -> String
identifier id =
    id


expression :: Expr -> String
expression e =
    case e of
        Value v ->
            printValue v

        Reference r ->
            r

        If condition whenTrue whenFalse ->
            "if " ++ expression condition ++ " then" ++ "\n"
                ++ (indent 1 <| expression whenTrue ++ "\n\n")
                ++ "else\n"
                ++ (indent 1 <| expression whenFalse ++ "\n")

        LetIn { Expression.definitions, Expression.body } ->
            "let\n"
                ++
                    ( definitions
                        |> NonEmpty.toList
                        |> map printDefinition
                        |> List.intercalate "\n\n"
                        |> indent 1
                    )
                ++ "\nin\n"
                ++ expression body


        CaseOf element cases ->
            "case " ++ expression element ++ " of\n"
                ++
                    ( cases
                        |> NonEmpty.toList
                        |> map printCase
                        |> List.intercalate "\n\n"
                        |> indent 1
                    )


        Lambda { Expression.params, Expression.body } ->
            "\\" ++ printParams params ++ " -> " ++ expression body

        Application { Expression.functionName, args } ->
            functionName ++ " " ++ printArgs args


printCase :: Case -> String
printCase (Case pattern expr) =
    printPattern pattern ++ " ->\n"
        ++ (indent 1 <| expression expr)


printPattern :: Pattern -> String
printPattern p =
    case p of
        WildCardPattern ->
            "_"

        ValuePattern v ->
            printValue v

        IdentifierPattern id ->
            id

        TuplePattern a b ->
            "( " ++ printPattern a ++ ", " ++ printPattern b ++ " )"

        Tuple3Pattern a b c ->
            "( " ++ printPattern a
                ++ ", " ++ printPattern b
                ++ ", " ++ printPattern c
            ++ " )"


printDefinition :: Definition -> String
printDefinition def =
    case def of
        SimpleDefinition name expr ->
            name ++ " =\n"
                ++ (indent 1 <| expression expr)

        _ ->
            "TODO"


printArgs :: NonEmpty Argument -> String
printArgs =
    let
        printArg arg =
            case arg of
                ValueArgument v ->
                    printValue v

                ReferenceArgument r ->
                    r

                ExpressionArgument e ->
                    parenthesized <| expression e
    in
    NonEmpty.toList
        >> map printArg
        >> String.unwords


parenthesized :: String -> String
parenthesized s =
    "(" ++ s ++ ")"



printParams :: NonEmpty Identifier -> String
printParams =
    String.unwords << NonEmpty.toList


printValue :: Value -> String
printValue v = case v of
        Bool TrueLiteral ->
            "True"

        Bool FalseLiteral ->
            "False"

        Char c ->
            [ c ]

        Float f ->
            show f

        Int n ->
            show n

        String s ->
            "\"" ++ s ++ "\""

