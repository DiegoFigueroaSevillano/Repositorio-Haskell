
data Exp = Val Int | Div Exp Exp

eval :: Exp -> Int
eval (Val x) = x
eval (Div x y) = div (eval x) (eval y)


