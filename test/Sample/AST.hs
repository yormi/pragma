module Sample.AST
    ( trueExpression
    ) where


import AST3.Expression (Expression)
import qualified AST3.Expression as Expression
import Parser3.Model.Quote (Quote(..))


trueExpression :: Quote -> Expression
trueExpression quote =
    Expression.TrueLiteral quote
        |> Expression.Bool
        |> Expression.Value
