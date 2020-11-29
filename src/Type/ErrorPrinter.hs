module Type.ErrorPrinter (printSolvingError) where

import qualified Data.List as List

import qualified AST.Expression as E
import qualified Printer
import qualified Type.Constraint.Model as Constraint
import Type.ConstraintSolver (SolvingError(..))


printSolvingError :: SolvingError -> String
printSolvingError e =
    case e of
        UnsolvableConstraint { expected, actual } ->
            show e


        TypeVariableCannotSatisfyBothConstraint typeA typeB ->
            show e


        IfConditionMustBeABool
            (Constraint.Element
                { Constraint.position
                , Constraint.expression
                , Constraint.type_
                }
            )
            ->
            errorHeader position
                [ "The if condition must be a Bool."
                , ""
                , "\tExpected :\t" ++ "Bool"
                , "\tActual :\t" ++ show type_
                , ""
                ]
                expression


        BothIfAlternativesMustHaveSameType whenTrue whenFalse ->
            show e


        NotAFunction
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


        BadApplication
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


        FunctionDefinitionMustMatchType functionType actualType ->
            show e


errorHeader :: E.Position -> [String] -> E.Expression -> String
errorHeader position errorExplaination expression =
    [
        [ ""
        , ""
        , "File: " ++ E.filename position
        , "Line: " ++ show (E.line position)
        , "Column: " ++ show (E.column position)
        , ""
        , ""
        ]
    ,
        errorExplaination
    , [ "", "" ]
    , codeQuote position expression
    --,
    --    [ ""
    --    , "---------------------------------------"
    --    ]
    ]
        |> List.concat
        |> map (\s -> "\t" ++ s)
        |> List.unlines


codeQuote :: E.Position -> E.Expression -> [String]
codeQuote position expression =
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
