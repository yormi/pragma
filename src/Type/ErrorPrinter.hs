module Type.ErrorPrinter (printSolvingError) where

import AST.CodeQuote (CodeQuote)
import qualified AST.CodeQuote as CodeQuote
import qualified Printer
import qualified Printer.Console as Console
import qualified Type as T
import Type.ConstraintSolver (SolvingError)
import qualified Type.ConstraintSolver as Solver
import qualified Utils.List as List
import qualified Utils.String as String


printSolvingError :: String -> SolvingError -> String
printSolvingError sourceCode e =
    case e of
        Solver.TypeVariableCannotSatisfyBothConstraint _ _ ->
            "TODO --- " ++ show e


        Solver.IfConditionMustBeABool { codeQuote, type_ } ->
            formatError
                sourceCode
                codeQuote
                "The type of the condition between the 'if' and the 'then' must be a Bool."
                [ "\tExpected:"
                , ""
                , "\t\t" ++ (Console.green <| Printer.printType T.Bool)
                , ""
                , "\tActual:"
                , ""
                , "\t\t" ++ (Console.red <| Printer.printType type_)
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
                        |> Printer.printType
                        |> Console.red
                    )
                , ""
                , "\tFalse:"
                , ""
                , "\t\t" ++
                    (whenFalse
                        |> Printer.printType
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
                (functionName ++ " must be a function if you want to pass arguments to it.")
                [ "\tActual :\t" ++ Printer.printType functionType ]


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
                    ++ functionName ++
                    " signature."
                )
                [ "\tSignature Type:"
                , ""
                , "\t\t" ++ Console.green (Printer.printType referenceType)
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
                , "\t\t" ++ Console.green (Printer.printType signatureType)
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
                        (Printer.printTypeAsParam actualArg)
                        ++ " -> "
                        ++ f a b

                (expectedType, actualType) ->
                    colorPrint
                        expectedType
                        actualType
                        (Printer.printTypeAsParam actualType)
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
