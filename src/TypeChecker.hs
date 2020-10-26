module TypeChecker
    ( check
    ) where


import qualified AST.Module as M
import AST.Module (Module)
import qualified Type as T
import TypeCheck.Check (Check)
import qualified TypeCheck.Check as Check
import qualified TypeCheck.Function as Function


check :: Module -> [Check T.Type]
check (M.Module topLevels) =
    let
        references =
            topLevels
                |> map
                    (\topLevel ->
                        case topLevel of
                            M.Function { M.functionName, M.type_ } ->
                                (functionName, type_)
                    )

    in
    topLevels
        |> map (\topLevel ->
            case topLevel of
                M.Function { M.type_, M.params, M.body } -> do
                    Check.withVariables references <|
                        Function.checkFunction type_ params body
        )
