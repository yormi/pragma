module Printer.Type.SolverError (print) where

import AST.CodeQuote (CodeQuote)
import qualified AST.CodeQuote as CodeQuote
import qualified AST.Identifier as Identifier
import qualified Printer.Type.Model as TypePrinter
import qualified Printer.Console as Console
import qualified Type.Model as T
import Type.Constraint.Solver.Model (SolvingError)
import qualified Type.Constraint.Solver.Model as Solver
import qualified Utils.List as List
import qualified Utils.String as String


print :: String -> SolvingError -> String
print sourceCode e =
    case e of
        Solver.TODO str ->
            "TODO --- " ++ str

        Solver.ShouldNotHappen str ->
            "SHOULD NOT HAPPEN --- This is a bug --- "
                ++ str
                ++ " --- "
                ++ show e

        Solver.TypeVariableCannotSatisfyBothConstraint _ _ ->
            "TODO --- " ++ show e


        Solver.IfConditionMustBeABool { codeQuote, type_ } ->
            formatError
                sourceCode
                codeQuote
                "The type of the condition between the 'if' and the 'then' must be a Bool."
                [ "\tExpected:"
                , ""
                , "\t\t" ++ (Console.green <| TypePrinter.print T.Bool)
                , ""
                , "\tActual:"
                , ""
                , "\t\t" ++ (Console.red <| TypePrinter.print type_)
                ]


        Solver.BothIfAlternativesMustHaveSameType
            { codeQuote
            , whenTrue
            , whenFalse
            }
            ->
            formatError sourceCode
                codeQuote
                "The if expressions must return the same type for both alternatives."
                [ "\tTrue:"
                , ""
                , "\t\t" ++
                    (whenTrue
                        |> TypePrinter.print
                        |> Console.red
                    )
                , ""
                , "\tFalse:"
                , ""
                , "\t\t" ++
                    (whenFalse
                        |> TypePrinter.print
                        |> Console.red
                    )
                ]


        Solver.NotAFunction
            { codeQuote
            , functionName
            , functionType
            }
            ->
            formatError
                sourceCode
                codeQuote
                (Identifier.formatReferenceId functionName
                    ++ " must be a function if you want to pass arguments to it.")
                [ "\tActual :\t" ++ TypePrinter.print functionType ]


        Solver.BadApplication
            { codeQuote
            , functionName
            , referenceType
            , functionType
            }
            ->
            formatError
                sourceCode
                codeQuote
                ("Arguments type must match with the type of "
                    ++ Identifier.formatReferenceId functionName
                    ++ " signature."
                )
                [ "\tSignature Type:"
                , ""
                , "\t\t" ++ Console.green (TypePrinter.print referenceType)
                , ""
                , "\tType according to the arguments:"
                , ""
                , "\t\t" ++ printComparedFunction referenceType functionType
                ]


        Solver.FunctionDefinitionMustMatchType
            { codeQuote
            , signatureType
            , definitionType
            }
            ->
            formatError
                sourceCode
                codeQuote
                "The function signature and the function definition types must be the same."
                [ "\tSignature Type:"
                , ""
                , "\t\t" ++ Console.green (TypePrinter.print signatureType)
                , ""
                , "\tDefinition Type:"
                , ""
                , "\t\t" ++ printComparedFunction signatureType definitionType
                ]


printComparedFunction :: T.Type -> T.Type -> String
printComparedFunction reference toPrint =
    let
        colorPrint expected actual =
            if actual == expected then
                Console.green
            else
                Console.red

        f refType applicationType =
            case (refType, applicationType) of
                ( T.Function (T.FunctionType expectedArg a)
                    , T.Function (T.FunctionType actualArg b)
                    ) ->
                    colorPrint
                        expectedArg
                        actualArg
                        (TypePrinter.printAsParam actualArg)
                        ++ " -> "
                        ++ f a b

                (expectedType, actualType) ->
                    colorPrint
                        expectedType
                        actualType
                        (TypePrinter.printAsParam actualType)
    in
    f reference toPrint

formatError :: String -> CodeQuote -> String -> [String] -> String
formatError sourceCode codeQuote whatItShouldBe errorExplaination =
    [
        [ ""
        , ""
        , "TYPE MISMATCH"
        , ""
        , "File: " ++ CodeQuote.filename (codeQuote :: CodeQuote)
        , ""
        , ""
        , whatItShouldBe
        , ""
        , ""
        ]
    ,
        errorExplaination
    , [ "", "", "" ]
    , formatCodeQuote sourceCode codeQuote
    ]
        |> List.concat
        |> map (\s -> "\t" ++ s)
        |> String.mergeLines


formatCodeQuote :: String -> CodeQuote -> [String]
formatCodeQuote sourceCode codeQuote =
    let
        toZeroBased n =
            n - 1

        firstLine =
            CodeQuote.fromLine codeQuote
                |> toZeroBased

        lastLine =
            CodeQuote.toLine codeQuote
                |> toZeroBased

    in
    sourceCode
        |> String.splitLines
        |> List.slice firstLine lastLine
        |> formatWithLineNumbers firstLine lastLine


formatWithLineNumbers :: Int -> Int -> [String] -> [String]
formatWithLineNumbers firstLine lastLine =
    List.foldl
        (\(lineNumber, result) line ->
            formatLine lastLine lineNumber line
                |> \l -> result ++ [l]
                |> \r -> (lineNumber + 1, r)
        )
        (firstLine, [])
        >> snd


formatLine :: Int -> Int -> String -> String
formatLine lastLine currentLineNumber line =
    let
        longestNumberLength =
            lastLine
                |> (show :: Int -> String)
                |> List.length

    in
    currentLineNumber
        |> show
        |> String.padLeft longestNumberLength
        |> (\s -> s ++ " |\t" ++ line)
