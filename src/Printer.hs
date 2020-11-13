module Printer (printModule, printType) where

import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.String as String

import AST.Expression
    ( BoolLiteral(..)
    , Case(..)
    , Definition(..)
    , Expr(..)
    , Identifier
    , Pattern(..)
    , Value(..)
    )
import qualified AST.Expression as Expression
import AST.Module (Module(..), TopLevel(..))
import qualified AST.Module as Module
import Type (Type)
import qualified Type


type Indentation = Int


printModule :: Module -> String
printModule (Module functions) =
    functions
        |> map topLevel
        |> List.intersperse "\n\n"
        |> String.unlines


topLevel :: TopLevel -> String
topLevel element =
    case element of
        Function
            { Module.type_
            , Module.functionName
            , Module.params
            , Module.body
            } ->
            let
                typeLine =
                    type_
                        |> printType
                        |> (\t -> functionName ++ " : " ++ t ++ "\n")


            in
            typeLine
                ++ functionName ++ " "
                ++ printParams params
                ++ " =\n"
                ++ (indent 1 <| expression body)


printType :: Type -> String
printType type_ =
    case type_ of
        Type.Bool ->
            "Bool"

        Type.Int ->
            "Int"

        Type.Float ->
            "Float"

        Type.Char ->
            "Char"

        Type.String ->
            "String"

        Type.Function (Type.FunctionType t1 t2) ->
            let
                formattedT1 =
                    case t1 of
                        Type.Function _ ->
                            t1
                                |> printType
                                |> parenthesized
                        _ ->
                            printType t1
            in
            formattedT1 ++ " -> " ++ printType t2

        Type.Variable n ->
            "a" <> show n


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
            "\\" ++ (printParams <| NonEmpty.toList <| params)
                ++ " -> " ++ expression body

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


printDefinition :: Definition -> String
printDefinition def =
    case def of
        SimpleDefinition name expr ->
            name ++ " =\n"
                ++ (indent 1 <| expression expr)


printArgs :: NonEmpty Expr -> String
printArgs =
    NonEmpty.toList
        >> map expression
        >> String.unwords


parenthesized :: String -> String
parenthesized s =
    "(" ++ s ++ ")"



printParams :: [Identifier] -> String
printParams =
    String.unwords


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

