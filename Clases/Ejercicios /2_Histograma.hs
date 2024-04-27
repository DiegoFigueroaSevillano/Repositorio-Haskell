module Histograma where

    --Restriccion 0 - 9 
    --  histrograma :: [Int] -> [String]

    {-
        Input: [7,4,2,4,5,6,7,5,4]
        Output: 
              *
        *     *
        * * * *
        0 1 2 3 4 5 6 7 8 9
    -}

    countNumber :: [Int] -> [(Int, Int)]
    countNumber xs = countNumberAux xs list
            where
                list :: [Int]
                list = [1..9]


    countNumberAux :: [Int] -> [Int] -> [(Int,Int)]
    countNumberAux _ [] = []
    countNumberAux xs (y:ys) = (y, countAux xs y):countNumberAux xs ys

    countAux :: [Int] -> Int -> Int
    countAux [] _ = 0
    countAux (x:xs) y = if x == y then 1 + countAux xs y else countAux xs y

    getHigher :: [(Int, Int)] -> Int
    getHigher [] = 0
    getHigher ((a,b):xs) = max b (getHigher xs)

    makeRow :: [(Int, Int)] -> Int -> [Int]
    makeRow [] _ = []
    makeRow ((x,y):xs) a = if y == a then 1:makeRow xs a else 0:makeRow xs a

    decrement :: [(Int, Int)] -> Int -> [(Int, Int)]
    decrement [] _ = []
    decrement ((x,y):xs) a = if y == a then (x,y-1):decrement xs a else (x,y):decrement xs a


    makeMatrix :: [(Int, Int)] -> Int -> [[Int]]
    makeMatrix _ 0 = []
    makeMatrix xs a = makeRow xs a:makeMatrix (decrement xs  a ) (a-1)

    printRow :: [Int] -> String
    printRow [] = ""
    printRow (x:xs) = if x == 1 then "*" ++ printRow xs else " " ++ printRow xs 

    printMatrix :: [[Int]] -> String
    printMatrix [] = "123456789 \n"
    printMatrix (x:xs) = printRow x ++ "\n" ++ printMatrix xs


    histograma :: [Int] -> String
    histograma xs = printMatrix (makeMatrix (countNumber xs) (getHigher (countNumber xs)))