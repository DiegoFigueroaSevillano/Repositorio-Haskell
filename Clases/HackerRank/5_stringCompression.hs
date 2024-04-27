
-- https://www.hackerrank.com/challenges/string-compression/problem (resuelto en la clase, sali a la pizarra para este)

module StringCompression where

    -- printLetter :: Char -> Int -> String
    -- printLetter l n = l : show n

    -- f :: String -> Char -> Int -> String
    -- f [] _ _ = ""
    -- f [x,y] l n = if (n == 0) && (x == y) then x : show 2 else [x,y]
    -- f (x:y:xs) l n = if x == y then f (y : xs) x (n+1) else if n == 0 then [x] ++ y:xs else (printLetter l (n+1)) ++ f xs '_' 0


    exercise :: String -> Int -> String
    exercise [] _ = []
    exercise [x] n = if n /= 0 then x : show (n+1) else [x]
    exercise (x:y:xs) n 
                | x == y = exercise (y:xs) (n+1)
                | n /= 0 = [x] ++ show(n+1) ++ exercise (y:xs) 0
                | otherwise = x:exercise(y:xs) 0

            
