module Context.Data
    ( Declaration(..)
    , context
    ) where


import qualified AST.Identifier as Identifier
import qualified AST.Module as M
import AST.TypeAnnotation (TypeAnnotation)
import Parser.Model.Quote (Quote)


data Declaration =
    Declaration
        { name :: String
        , quote :: Quote
        , annotation :: TypeAnnotation
        }
        deriving (Eq, Show)


context :: [M.TopLevel] -> [Declaration]
context =
    bind
        (\topLevel ->
            case topLevel of
                M.Function { M.functionName, M.typeAnnotation } ->
                    let
                        name =
                            Identifier.formatDataId functionName

                        quote =
                            M.topLevelQuote topLevel
                    in
                    [Declaration name quote typeAnnotation]

                M.SumType {} -> do
                    []

                M.Record {} -> do
                    []
        )
