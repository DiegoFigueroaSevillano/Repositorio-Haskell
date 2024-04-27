-- STRING MINGLING: https://www.hackerrank.com/challenges/string-mingling/problem 

module StringMingling where 

    f :: String -> String -> String
    f [] [] = []
    f xs [] = xs
    f [] ys = ys
    f (x:xs) (y:ys) = [x] ++ [y] ++ f xs ys

    a :: String
    a = "abcde"
    b :: String 
    b = "pqrst"
