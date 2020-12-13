module Type.ErrorPrinter (printSolvingError) where

import AST.CodeQuote (CodeQuote, Position)
import qualified AST.CodeQuote as CodeQuote
import qualified AST.Expression as E
import qualified Printer
import qualified Printer.Console as Console
import qualified Type as T
import qualified Type.Constraint.Model as Constraint
import Type.ConstraintSolver (SolvingError)
import qualified Type.ConstraintSolver as Solver
import qualified Utils.List as List
import qualified Utils.String as String


printSolvingError :: String -> SolvingError -> String
printSolvingError sourceCode e =
    case e of
        Solver.UnsolvableConstraint { expected, actual } ->
            show e


        Solver.TypeVariableCannotSatisfyBothConstraint typeA typeB ->
            show e


        Solver.IfConditionMustBeABool
            (Constraint.Element
                { Constraint.position
                , Constraint.expression
                , Constraint.type_
                }
            )
            ->
            errorHeader position
                [ "The condition type in `if condition then` must be a Bool."
                , ""
                , "\tExpected:"
                , ""
                , "\t\t" ++ (Console.green <| Printer.printType T.Bool)
                , ""
                , "\tActual:"
                , ""
                , "\t\t" ++ (Console.red <| Printer.printType type_)
                , ""
                ]
                expression


        Solver.BothIfAlternativesMustHaveSameType
            { codeQuote
            , whenTrue
            , whenFalse
            }
            ->
            errorHeader2 sourceCode
                codeQuote
                "The if expressions must return the same type for both alternatives."
                [ "\tTrue:"
                , ""
                , "\t\t" ++
                    (Constraint.type_ whenTrue
                        |> Printer.printType
                        |> Console.red
                    )
                , ""

                , "\t\t" ++
                    (Constraint.type_ whenFalse
                        |> Printer.printType
                        |> Console.red
                    )
                , ""
                , "Which type do you want your structure to return ?"
                ]


        Solver.NotAFunction
            { codeQuote
            , functionName
            , functionType
            }
            ->
            errorHeader2
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
            errorHeader2
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
            errorHeader2
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

errorHeader2 :: String -> CodeQuote -> String -> [String] -> String
errorHeader2 sourceCode codeQuote whatItShouldBe errorExplaination =
    [
        [ ""
        , ""
        , "File: " ++ CodeQuote.filename (codeQuote :: CodeQuote)
        , ""
        , ""
        , "TYPE MISMATCH"
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



errorHeader :: Position -> [String] -> E.Expression -> String
errorHeader position errorExplaination expression =
    [
        [ ""
        , ""
        , "File: " ++ CodeQuote.filename (position :: CodeQuote.Position)
        , "Line: " ++ show (CodeQuote.line position)
        , "Column: " ++ show (CodeQuote.column position)
        , ""
        , ""
        ]
    ,
        errorExplaination
    , [ "", "" ]
    , formatExpression position expression
    --,
    --    [ ""
    --    , "---------------------------------------"
    --    ]
    ]
        |> List.concat
        |> map (\s -> "\t" ++ s)
        |> String.mergeLines


formatExpression :: Position -> E.Expression -> [String]
formatExpression position expression =
    let
        longerNumber =
            position
                |> CodeQuote.line
                |> (\n -> n + 1 )
                |> (show :: Int -> String)
                |> List.length

        formatLineNumber n =
            n
                |> show
                |> (\s ->
                    List.replicate (longerNumber - List.length s) ' ' ++ s)
                |> (\s -> s ++ " |")
    in
        [ formatLineNumber (CodeQuote.line position - 1)
        ,
            formatLineNumber (CodeQuote.line position) ++ "\t"
             ++ Printer.printExpression (E.Expr position expression)
        , formatLineNumber (CodeQuote.line position + 1)
        ]
