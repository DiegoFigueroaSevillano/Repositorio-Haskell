
data Exp = Val Int | Div Exp Exp

eval :: Exp -> Maybe Int
eval (Val x) = Just x
eval (Div x y) = do 
                    n <- eval x
                    m <- eval y
                    safeDiv n m
                            


safeDiv :: Int -> Int -> Maybe Int
safeDiv x y = if y == 0 
                then Nothing 
                else Just (div x y)


