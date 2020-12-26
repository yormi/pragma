module Printer.AST.Expression
    ( print
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
    , Pattern(..)
    , Value(..)
    )
import qualified AST.Expression as Expression
import AST.Identifier (DataId)
import qualified AST.Identifier as Identifier
import qualified Printer.Utils as Utils
import qualified Utils.String as String


print :: QuotedExpression -> String
print e =
    case Expression.expression e of
        Value v ->
            printValue v

        Reference r ->
            Identifier.formatReferenceId r

        If condition whenTrue whenFalse ->
            "if " ++ print condition ++ " then" ++ "\n"
                ++ (Utils.indent <| print whenTrue ++ "\n\n")
                ++ "else\n"
                ++ (Utils.indent <| print whenFalse ++ "\n")

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
                ++ print body


        CaseOf element cases ->
            "case " ++ print element ++ " of\n"
                ++
                    ( cases
                        |> NonEmpty.toList
                        |> map printCase
                        |> List.intercalate "\n\n"
                        |> Utils.indent
                    )


        Lambda { Expression.params, Expression.body } ->
            "\\" ++ (printParams <| NonEmpty.toList <| params)
                ++ " -> " ++ print body

        Application { Expression.functionName, args } ->
            Identifier.formatReferenceId functionName ++ " " ++ printArgs args


printCase :: Case -> String
printCase (Case pattern expr) =
    printPattern pattern ++ " ->\n"
        ++ (Utils.indent <| print expr)


printPattern :: Pattern -> String
printPattern p =
    case p of
        WildCardPattern ->
            "_"

        ValuePattern v ->
            printValue v

        IdentifierPattern id ->
            Identifier.formatDataId id

        TuplePattern a b ->
            "( " ++ printPattern a ++ ", " ++ printPattern b ++ " )"


printDefinition :: Definition -> String
printDefinition def =
    case def of
        SimpleDefinition name expr ->
            Identifier.formatDataId name ++ " =\n"
                ++ (Utils.indent <| print expr)


printArgs :: NonEmpty QuotedExpression -> String
printArgs =
    NonEmpty.toList
        >> map print
        >> map Utils.parenthesizeIfHasSpace
        >> String.unwords


printParams :: [DataId] -> String
printParams =
    map Identifier.formatDataId
        >> String.mergeWords


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

