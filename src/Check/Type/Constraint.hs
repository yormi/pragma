module Check.Type.Constraint
    ( Constraint(..)
    , build
    ) where


import AST.TypeAnnotation (TypeAnnotation)
import qualified Check.Type.Arrange as A
import qualified Check.Type.Model.PrimitiveType as Primitive
import Parser.Model.Quote (Quote)


data Constraint
    = Primitive
        { link :: A.Link
        , quote :: Quote
        , primitiveType :: Primitive.Type
        }
    | ContextReference
        { link :: A.Link
        , annotation :: TypeAnnotation
        }
    | Future
        { link :: A.Link
        }
    | Definition
        { link :: A.Link
        , bodyLink :: A.Link
        }
    | IfCondition A.Link
    | IfAlternatives
        { whenTrue :: A.Link
        , whenFalse :: A.Link
        }
    | Redirect
        { from :: A.Link
        , to :: A.Link
        }
    deriving (Eq, Show)


-- ACTION


build :: [ A.Expression ] -> [ Constraint ]
build =
    bind fromArrangedExpression


fromArrangedExpression :: A.Expression -> [ Constraint ]
fromArrangedExpression expression =
    case expression of
        A.Primitive { link, quote, primitiveType } ->
            [ Primitive link quote primitiveType ]

        A.ContextReference { link , annotation } ->
            [ ContextReference link annotation ]

        A.Future { link } ->
            [ Future link ]

        A.Definition { link, bodyLink } ->
            [ Definition link bodyLink ]

        A.If { condition, whenTrue, whenFalse, returns } ->
            [ IfCondition condition
            , IfAlternatives whenTrue whenFalse
            , Redirect returns whenTrue
            ]
