module Printer
    ( printType
    , printTypeAsParam
    , printTypeSolution
    , printExpression
    ) where

import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.String as String

import AST.Expression
    ( BoolLiteral(..)
    , Case(..)
    , Definition(..)
    , QuotedExpression
    , Expression(..)
    , Identifier
    , Pattern(..)
    , Value(..)
    )
import qualified AST.Expression as Expression
import qualified Printer.Utils as Utils
import Type (Type)
import qualified Type
import qualified Type.Constraint.Solver.Model as Solver


printTypeAsParam :: Type -> String
printTypeAsParam type_ =
    case type_ of
        Type.Function _ ->
            "(" ++ printType type_ ++ ")"

        _ ->
            printType type_


printTypeSolution :: Solver.SolutionType -> String
printTypeSolution solution =
    case solution of
        Solver.InstanceType type_ ->
            printType type_

        Solver.NamedType genericVariables type_ ->
            let
                formattedGenericVariable :: String
                formattedGenericVariable =
                    genericVariables
                        |> map Type.Variable
                        |> map printType
                        |> List.intercalate ", "
            in
            "∀ " ++ formattedGenericVariable ++ " | " ++ printType type_


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
                                |> Utils.parenthesized
                        _ ->
                            printType t1
            in
            formattedT1 ++ " -> " ++ printType t2

        Type.Variable n ->
            "a" <> show n


printExpression :: QuotedExpression -> String
printExpression e =
    case Expression.expression e of
        Value v ->
            printValue v

        Reference r ->
            r

        If condition whenTrue whenFalse ->
            "if " ++ printExpression condition ++ " then" ++ "\n"
                ++ (Utils.indent <| printExpression whenTrue ++ "\n\n")
                ++ "else\n"
                ++ (Utils.indent <| printExpression whenFalse ++ "\n")

        LetIn { Expression.definitions, Expression.body } ->
            "let\n"
                ++
                    ( definitions
                        |> NonEmpty.toList
                        |> map printDefinition
                        |> List.intercalate "\n\n"
                        |> Utils.indent
                    )
                ++ "\nin\n"
                ++ printExpression body


        CaseOf element cases ->
            "case " ++ printExpression element ++ " of\n"
                ++
                    ( cases
                        |> NonEmpty.toList
                        |> map printCase
                        |> List.intercalate "\n\n"
                        |> Utils.indent
                    )


        Lambda { Expression.params, Expression.body } ->
            "\\" ++ (printParams <| NonEmpty.toList <| params)
                ++ " -> " ++ printExpression body

        Application { Expression.functionName, args } ->
            functionName ++ " " ++ printArgs args


printCase :: Case -> String
printCase (Case pattern expr) =
    printPattern pattern ++ " ->\n"
        ++ (Utils.indent <| printExpression expr)


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
                ++ (Utils.indent <| printExpression expr)


printArgs :: NonEmpty QuotedExpression -> String
printArgs =
    NonEmpty.toList
        >> map printExpression
        >> String.unwords


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

