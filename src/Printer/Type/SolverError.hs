module Printer.Type.SolverError (print) where

import AST.CodeQuote (CodeQuote)
import qualified AST.CodeQuote as CodeQuote
import qualified AST.Identifier as Identifier
import qualified Printer.AST.TypeAnnotation as TypeAnnotationPrinter
import qualified Printer.Console as Console
import qualified Printer.Type.Instanced as InstancedTypePrinter
import qualified Printer.Type.Solution as SolutionPrinter
import qualified Printer.Utils as Utils
import qualified Type.Constraint.Solver.Model.Instanced as I
import Type.Constraint.Solver.Model.Solution (Solution)
import Type.Constraint.Solver.Model.Solver (SolvingError)
import qualified Type.Constraint.Solver.Model.Solver as Solver
import qualified Utils.List as List
import qualified Utils.String as String



print :: String -> SolvingError -> String
print sourceCode e =
    case e of
        Solver.TODO str ->
            "TODO --- " ++ str

        Solver.ShouldNotHappen str _ ->
            "SHOULD NOT HAPPEN --- This is a bug --- "
                ++ str
                ++ " --- "
                ++ show e

        Solver.TypeVariableCannotSatisfyBothConstraint _ _ _ ->
            "TODO --- " ++ show e


        Solver.IfConditionMustBeABool { codeQuote, type_, solutionSoFar } ->
            formatError
            solutionSoFar
                sourceCode
                codeQuote
                "The type of the condition between the 'if' and the 'then' must be a Bool."
                [ "\tExpected:"
                , ""
                , "\t\t" ++ (Console.green <| InstancedTypePrinter.print I.Bool)
                , ""
                , "\tActual:"
                , ""
                , "\t\t" ++ (Console.red <| InstancedTypePrinter.print type_)
                ]


        Solver.BothIfAlternativesMustHaveSameType
            { codeQuote
            , whenTrue
            , whenFalse
            , solutionSoFar
            }
            ->
            formatError
                solutionSoFar
                sourceCode
                codeQuote
                "The if expressions must return the same type for both alternatives."
                [ "\tTrue:"
                , ""
                , "\t\t" ++
                    (whenTrue
                        |> InstancedTypePrinter.print
                        |> Console.red
                    )
                , ""
                , "\tFalse:"
                , ""
                , "\t\t" ++
                    (whenFalse
                        |> InstancedTypePrinter.print
                        |> Console.red
                    )
                ]


        Solver.BadApplication
            { codeQuote
            , functionName
            , referenceType
            , functionType
            , solutionSoFar
            }
            ->
            case functionType of
                I.Function _ _ ->
                    formatError
                        solutionSoFar
                        sourceCode
                        codeQuote
                        ("Arguments type must match with the type of "
                            ++ Identifier.formatReferenceId functionName
                            ++ " signature."
                        )
                        [ "\tSignature Type:"
                        , ""
                        , "\t\t" ++ Console.green (InstancedTypePrinter.print referenceType)
                        , ""
                        , "\tType according to the arguments:"
                        , ""
                        , "\t\t" ++ printComparedFunction referenceType functionType
                        ]

                _ ->
                    formatError
                        solutionSoFar
                        sourceCode
                        codeQuote
                        (Identifier.formatReferenceId functionName
                            ++ " must be a function if you want to pass arguments to it.")
                        [ "\tActual :\t" ++ InstancedTypePrinter.print functionType ]

        Solver.FunctionDefinitionMustMatchType
            { codeQuote
            , signatureType
            , definitionType
            , solutionSoFar
            }
            ->
            formatError
                solutionSoFar
                sourceCode
                codeQuote
                "The function signature and the function definition types must be the same."
                [ "\tSignature Type:"
                , ""
                , "\t\t" ++ Console.green (TypeAnnotationPrinter.print signatureType)
                , ""
                , "\tDefinition Type:"
                , ""
                , "\t\t" ++ Console.red (InstancedTypePrinter.print definitionType)
                ]


printComparedFunction :: I.InstancedType -> I.InstancedType -> String
printComparedFunction reference toPrint =
    let
        colorPrint expected actual =
            if actual == expected then
                Console.green
            else
                Console.red

        f refType applicationType =
            case (refType, applicationType) of
                ( I.Function expectedArg a, I.Function actualArg b) ->
                    colorPrint
                        expectedArg
                        actualArg
                        (actualArg
                            |> InstancedTypePrinter.print
                            |> Utils.parenthesizeIfFunction
                        )
                        ++ " -> "
                        ++ f a b

                (expectedType, actualType) ->
                    colorPrint
                        expectedType
                        actualType
                        (actualType
                            |> InstancedTypePrinter.print
                            |> Utils.parenthesizeIfFunction
                        )
    in
    f reference toPrint

formatError :: Solution -> String -> CodeQuote -> String -> [String] -> String
formatError solutionSoFar sourceCode codeQuote whatItShouldBe errorExplaination =
    [ [ SolutionPrinter.print solutionSoFar ]
    ,
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
