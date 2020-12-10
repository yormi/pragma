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
                , Console.green <| "\tExpected :\t" ++ "Bool"
                , Console.red <| "\tActual :\t" ++ show type_
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
                [ "The if expressions must return the same type for both alternatives."
                , ""
                , Console.red
                    ("\tTrue :\t" ++ show (Constraint.type_ whenTrue))
                , Console.red
                    ("\tFalse :\t" ++ show (Constraint.type_ whenFalse))
                , ""
                , "Which type do you want your structure to return ?"
                ]


        Solver.NotAFunction
            { codeQuote
            , functionName
            , args
            , functionType
            }
            ->
            errorHeader2
                sourceCode
                codeQuote
                [ functionName ++ " must be a function if you want to pass arguments to it."
                , ""
                , "\tActual :\t" ++ Printer.printType functionType
                ]


        Solver.BadApplication
            { codeQuote
            , functionName
            , referenceType
            , functionType
            } ->
            errorHeader2
                sourceCode
                codeQuote
                [ "Arguments does not match with the type of "
                    ++ functionName
                , ""
                , ""
                , "The function type is:"
                , ""
                , "REPLACE PRINTER to make arrow white and non-matching type red"
                , "\t" ++ Console.green (Printer.printType referenceType)
                , ""
                , "While according to the arguments, the function type should be:"
                , ""
                , let
                    f refType applicationType =
                        case (refType, applicationType) of
                            ( T.Function (T.FunctionType expectedArg a)
                                , T.Function (T.FunctionType actualArg b)
                                ) ->
                                let
                                    color =
                                        if actualArg == expectedArg then
                                            Console.green
                                        else
                                            Console.red

                                in
                                color (Printer.printType actualArg)
                                    ++ " -> "
                                    ++ f a b

                            ( _, actualArg) ->
                                Console.green (Printer.printType actualArg)
                in
                "\t" ++ (f referenceType functionType)
                ]


        Solver.FunctionDefinitionMustMatchType functionType actualType ->
            show e


errorHeader2 :: String -> CodeQuote -> [String] -> String
errorHeader2 sourceCode codeQuote errorExplaination =
    [
        [ ""
        , ""
        , "File: " ++ CodeQuote.filename (codeQuote :: CodeQuote)
        , ""
        , ""
        ]
    ,
        errorExplaination
    , [ "", "" ]
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
