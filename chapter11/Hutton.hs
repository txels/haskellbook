module Hutton where


data Expr
    = Lit Integer
    | Add Expr Expr
    deriving (Eq, Show)

eval :: Expr -> Integer
eval (Lit x) = x
eval (Add x y) = (eval x) + (eval y)

printExpr :: Expr -> String
printExpr (Lit x) = show x
printExpr (Add x y) = printExpr x ++ " + " ++ printExpr y
