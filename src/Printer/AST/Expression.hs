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
    , Expression(..)
    , Pattern(..)
    , Value(..)
    )
import AST.Identifier (DataId)
import qualified AST.Identifier as Identifier
import qualified Printer.Utils as Utils
import qualified Utils.String as String


print :: Expression -> String
print expression =
    case expression of
        Value v ->
            printValue v

        Reference r ->
            Identifier.formatReferenceId r

        If _ condition whenTrue whenFalse ->
            [ "if " ++ print condition ++ " then"
            , Utils.indent <| print whenTrue
            , ""
            , "else"
            , Utils.indent <| print whenFalse
            ]
                |> String.mergeLines

        LetIn { definitions, body } ->
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


        CaseOf { element, cases } ->
            "case " ++ print element ++ " of\n"
                ++
                    ( cases
                        |> NonEmpty.toList
                        |> map printCase
                        |> List.intercalate "\n\n"
                        |> Utils.indent
                    )


        Lambda { params, body } ->
            "\\" ++ (printParams <| NonEmpty.toList <| params)
                ++ " -> " ++ print body

        Application { functionName, args } ->
            Identifier.formatReferenceId functionName ++ " " ++ printArgs args


printCase :: Case -> String
printCase (Case pattern_ expr) =
    printPattern pattern_ ++ " ->\n"
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


printArgs :: NonEmpty Expression -> String
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
        Bool (TrueLiteral _) ->
            "True"

        Bool (FalseLiteral _) ->
            "False"

        Char _ c ->
            [ c ]

        Float _ f ->
            show f

        Int _ n ->
            show n

        String _ s ->
            show s

