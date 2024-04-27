-- SUPER DIGIT : https://www.hackerrank.com/challenges/super-digit/problem 

module SuperDigit where 

    repeatNumber :: [Int] -> Int -> [Int]
    repeatNumber _ 0 = []
    repeatNumber xs a = xs ++ repeatNumber xs (a-1)

    splitNumber :: Int -> [Int]
    splitNumber a 
                | a > 9 = splitNumber (div a 10) ++ [mod a 10]
                | otherwise = [a]

    superDigitAux :: [Int] -> Int
    superDigitAux xs 
                | length xs > 1 = superDigitAux (splitNumber (sum xs))
                | otherwise = head xs

    superDigit :: Int -> Int -> Int
    superDigit a b = superDigitAux (repeatNumber (splitNumber a) b)

    
