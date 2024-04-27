module CardValidation where

    toDigits :: Int -> [Int]
    toDigits 0 = []
    toDigits a
            | a < 0 = []
            | a < 10 = [a]
            | otherwise = toDigits (div a 10) ++ [mod a  10 ]

    toDigitsRev :: Int -> [Int]
    toDigitsRev 0 = []
    toDigitsRev a
            | a < 0 = []
            | a < 10 = [a]
            | otherwise = mod a  10 : toDigitsRev (div a 10)


    doubleEveryOther :: [Int] -> Bool -> [Int]
    doubleEveryOther [] _ = []
    doubleEveryOther (x:xs) a  = if a then doubleEveryOther xs False ++ [x * 2] else doubleEveryOther xs True ++ [x]

    sumDigits :: [Int] -> Int
    sumDigits xs = sum $ digits xs
            where
                digits :: [Int] -> [Int]
                digits [] = []
                digits (x:xs) = if x > 9 then toDigits x ++ digits xs else x : digits xs

    validate :: Int -> Bool
    validate a = mod (sumDigits (doubleEveryOther (toDigitsRev a) False)) 10 == 0
