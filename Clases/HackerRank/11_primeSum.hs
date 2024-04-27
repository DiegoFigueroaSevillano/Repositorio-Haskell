-- PRIME SUM : https://www.hackerrank.com/challenges/prime-sum/problem 

module PrimeSum where 

    esPrimo :: Int -> Bool
    esPrimo 1 = False
    esPrimo a = length (filter (\x -> rem a x == 0) list) == 1
        where
            list :: [Int]
            list = [2..a]

    getPrimos :: Int -> [Int]
    getPrimos a = filter esPrimo [1..a]

    eliminateValue :: [Int] -> Int -> [Int]
    eliminateValue (x:xs) a 
                        | a == x = xs
                        | otherwise = x : eliminateValue xs a  

    eliminateListValue :: [Int] -> [Int] -> [Int]
    eliminateListValue xs [] = xs
    eliminateListValue xs (y:ys) = eliminateListValue (eliminateValue xs y) ys

    
    sumPrimAux :: [Int] -> [Int] -> Bool -> Int -> Int -> Int -> Bool
    sumPrimAux [] _ True _ _ _ = True
    sumPrimAux (x:xs) ys z e v c  
                        | c > v && e /= 0 = False
                        | c == v && e == 0 = True
                        | otherwise = sumPrimAux (eliminateValue ys x) (eliminateValue ys x) z (e - x) v (c+1)