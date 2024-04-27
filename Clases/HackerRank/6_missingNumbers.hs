
--MISSING NUMBERS: https://www.hackerrank.com/challenges/missing-numbers/problem 

module MissingNumber where
    import Data.List (sort)


    deleteNumber :: [Int] -> Int -> Bool -> [Int]
    deleteNumber [] _ _ = []
    deleteNumber (x:xs) n b
                        | not b = if x == n then deleteNumber xs n True else x:deleteNumber xs n False
                        | otherwise = x:xs
    
    exercise :: [Int] -> [Int] -> [Int] -> [Int]
    exercise [] _ zs = zs
    exercise (x:xs) ys zs = exercise xs (deleteNumber ys x False) (deleteNumber ys x False)

    result :: [Int] -> [Int] -> [Int]
    result xs ys = sort $ exercise xs ys []

    inputA :: [Int]
    inputA = [203, 204, 205, 206, 207, 208, 203, 204, 205, 206]

    inputB :: [Int]
    inputB = [203, 204, 204, 205, 206, 207, 205, 208, 203, 206, 205, 206, 204]