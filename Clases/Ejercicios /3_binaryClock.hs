module BinaryClock where
    import Data.Char (digitToInt)
    
    arrayToDec :: [Int] -> Int
    arrayToDec xs = arrayToDecAux xs 0

    arrayToDecAux :: [Int] -> Int -> Int
    arrayToDecAux [] _ = 0
    arrayToDecAux (x:xs) aux
                    | x /= 0 =  2 ^ aux + arrayToDecAux xs (aux+1)
                    | otherwise = arrayToDecAux xs (aux+1)

    getArray :: String -> [[Int]]
    getArray "" = []
    getArray a = map digitToInt (take 6 a) : getArray (drop 6 a)

    splitArray :: [[Int]] -> Int -> [[Int]]
    splitArray _ 6 = []
    splitArray xs a = getPart xs a:splitArray xs (a+1)

    getPart :: [[Int]] -> Int -> [Int]
    getPart [] _ = []
    getPart (x:xs) a = (x !! a):getPart xs a

    printHour :: [[Int]] -> String
    printHour xs = show (arrayToDec ( reverse $ xs !! 0)) ++ show (arrayToDec (reverse $ xs !! 1))  ++ ":" ++ show (arrayToDec (reverse $ xs !! 2)) ++ show (arrayToDec (reverse $ xs !! 3)) ++ ":" ++ show (arrayToDec (reverse $ xs !! 4)) ++ show (arrayToDec (reverse $ xs !! 5))

    getHour :: String -> String
    getHour a = printHour (splitArray (getArray a) 0)


    example1 :: String
    example1 = getHour  "000000000000000000010101"

    example2 :: String
    example2 = getHour "000000010101001011100110"