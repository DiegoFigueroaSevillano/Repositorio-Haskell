
-- EXPRESSIONS : https://www.hackerrank.com/challenges/expressions-v2/problem 

module ExpressionsV2 where

    data Exp = Unary Int
            | Add Exp Exp
            | Sus Exp Exp
            | Mul Exp Exp
            | Div Exp Exp
            | Bracket Exp
            deriving Eq

    instance Show Exp where
        show (Unary n)
                        | n >= 0 = show n
                        | n < 0 = "("++show n++")"
        show (Add a b) = show a ++ " + " ++ show b
        show (Sus a b) = show a ++ " - " ++ show b
        show (Mul a b) = show a ++ " * " ++ show b
        show (Div a b) = show a ++ " / " ++ show b
        show (Bracket e) = "(" ++ show e ++ ")"

    resolve :: Exp -> Int
    resolve (Unary e) = e
    resolve (Bracket e) = resolve e
    resolve (Add a b) = resolve a + resolve b
    resolve (Sus a b) = resolve a - resolve b
    resolve (Mul a b) = resolve a * resolve b
    resolve (Div a b) = div (resolve a) (resolve b)

    expresion :: Exp
    expresion = Mul (Bracket (Add (Unary 5) (Unary (-4)))) (Unary 3)

    isOperation :: Char -> Bool
    isOperation o
                | o =='+' = True
                | o == '-' = True
                | o == '*' = True
                | o == '/' = True
                | o == ')' = True
                | o == '(' = True
                | otherwise = False

    stringToList :: String -> String -> [String]
    stringToList [] a = [a | a /= ""]
    stringToList (x:xs) a
                    | x == ' ' = stringToList xs a
                    | isOperation x = if a /= "" then [a] ++ [[x]] ++ stringToList xs "" else [x] : stringToList xs ""
                    | otherwise = stringToList xs (a ++ [x])

    toExp :: [String] -> Exp
    toExp [x] = Unary (read x)
    toExp xs
        | length xs == 1 = Unary (read (head xs))
        | otherwise = toExp' xs []
        where
            toExp' :: [String] -> [Exp] -> Exp
            toExp' [] [e] = e
            toExp' (x:xs) stack
                | x `elem` ["+", "-"] = toExp' xs (Unary (read x) : stack)
                | x == "*" = toExp' xs (Mul (last stack) (Unary (read (head xs))) : init stack)
                | x == "/" = toExp' xs (Div (last stack) (Unary (read (head xs))) : init stack)
                | x == "(" = toExp' xs (Bracket (toExp (takeBracket xs)) : stack)
                | x == ")" = toExp' xs (resolveBracket stack)
                | otherwise = toExp' xs (Unary (read x) : stack)
            
            takeBracket :: [String] -> [String]
            takeBracket (x:xs)
                | x == ")" = checkNext xs
                | x == "(" = takeBracket' xs
                | otherwise = x : takeBracket xs
                where
                    checkNext :: [String] -> [String]
                    checkNext (x:xs)
                        | otherwise = x : xs

                    takeBracket' :: [String] -> [String]
                    takeBracket' (x:xs)
                        | x == ")" = []
                        | otherwise = x : takeBracket' xs

            resolveBracket :: [Exp] -> [Exp]
            resolveBracket (x:xs)
                | isOperation (head (show x)) = resolveBracket xs
                | otherwise = (x:xs)

    example :: String -> Int
    example input = resolve $ toExp $ stringToList input ""