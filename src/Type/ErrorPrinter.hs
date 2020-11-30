module Type.ErrorPrinter (printSolvingError) where

import qualified AST.Expression as E
import qualified Printer
import qualified Printer.Console as Console
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
            { position
            , functionName
            , args
            , functionType
            }
            ->
            errorHeader position
                [ functionName ++ " must be a function if you want to pass arguments to it."
                , ""
                , "\tActual :\t" ++ Printer.printType functionType
                ]
                (E.Application functionName args)


        Solver.BadApplication
            { position
            , functionName
            , args
            , referenceType
            , functionType
            } ->
            errorHeader position
                [ "Arguments does not match with "
                    ++ functionName
                    ++ " type."
                , ""
                , ""
                , "\tExpected :\t" ++ Printer.printType referenceType
                , "\tActual :\t" ++ Printer.printType functionType
                ]
                (E.Application functionName args)


        Solver.FunctionDefinitionMustMatchType functionType actualType ->
            show e


errorHeader2 :: String -> E.CodeQuote -> [String] -> String
errorHeader2 sourceCode codeQuote errorExplaination =
    [
        [ ""
        , ""
        , "File: " ++ E.filename (codeQuote :: E.CodeQuote)
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


formatCodeQuote :: String -> E.CodeQuote -> [String]
formatCodeQuote sourceCode codeQuote =
    let
        toZeroBased n =
            n - 1

        firstLine =
            E.fromLine codeQuote
                |> toZeroBased

        lastLine =
            E.toLine codeQuote
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



errorHeader :: E.Position -> [String] -> E.Expression -> String
errorHeader position errorExplaination expression =
    [
        [ ""
        , ""
        , "File: " ++ E.filename (position :: E.Position)
        , "Line: " ++ show (E.line position)
        , "Column: " ++ show (E.column position)
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


formatExpression :: E.Position -> E.Expression -> [String]
formatExpression position expression =
    let
        longerNumber =
            position
                |> E.line
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
        [ formatLineNumber (E.line position - 1)
        ,
            formatLineNumber (E.line position) ++ "\t"
             ++ Printer.printExpression (E.Expr position expression)
        , formatLineNumber (E.line position + 1)
        ]
