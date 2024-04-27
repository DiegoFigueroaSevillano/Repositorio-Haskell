
data Exp = Val Int | Div Exp Exp

eval :: Exp -> Maybe Int
eval (Val x) = Just x
eval (Div x y) = case (eval x) of 
                    Nothing -> Nothing
                    Just x -> case (eval y) of
                                Nothing -> Nothing
                                Just y -> safeDiv x y
                            


safeDiv :: Int -> Int -> Maybe Int
safeDiv x y = if y == 0 
                then Nothing 
                else Just (div x y)


