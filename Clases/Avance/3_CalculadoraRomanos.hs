module CalculadoraRomana where

    romanBase :: Char -> Int
    romanBase r = case r of
                        'I' -> 1
                        'V' -> 5
                        'X' -> 10
                        'L' -> 50
                        'C' -> 100
                        'D'-> 500
                        'M' -> 1000
                        _ -> error "NO ES UN ROMANO"


    -- IV
    -- romanToInt :: String -> Int -> Int
    -- romanToInt (x:xs) = if (length xs == 1) then  if romanBase (head xs) < romanBase x then romanBase (head xs) + romanBase x else romanBase (head xs) - romanBase x  else if romanBase x < romanBase (head xs) then romanBase x - romanToInt xs else romanToInt xs + romanBase x

    romanToInt :: String -> Int -> Int
    romanToInt [x] _ = romanBase x
    romanToInt [x,y] a
                    | a == 0 = if romanBase x > romanBase y then romanBase x - romanBase y else romanBase y + romanBase x
                    | otherwise = if romanBase x > romanBase y then -romanBase y else romanBase y
    romanToInt (x:y:xs) a
                    | a == 0 = if romanBase x > romanBase y then romanBase x - romanBase y + romanToInt (y:xs) (romanBase x - romanBase y) else romanBase y + romanBase x + romanToInt (y:xs) (romanBase x + romanBase y)
                    | otherwise = if romanBase x > romanBase y then -romanBase y + romanToInt (y:xs) (a - romanBase y) else romanBase y + romanToInt (y:xs) (a + romanBase y)


    conversorRomanToInt :: String -> Int
    conversorRomanToInt a 
                        | head a == '-' = - romanToInt (revertString$ tail a) 0
                        | otherwise = romanToInt (revertString a) 0

    revertString :: String -> String
    revertString [] = []
    revertString (x:xs) = revertString xs ++ [x]


    decimalBase :: [(String, Int)]
    decimalBase = [("I", 1), ("V", 5), ("X", 10), ("L", 50), ("C", 100), ("D", 500), ("M", 1000)]

    splitNumber :: Int -> [Int]
    splitNumber a = splitNumberAux a 1
            where
                splitNumberAux :: Int -> Int -> [Int]
                splitNumberAux 0 _ = []
                splitNumberAux a b = splitNumberAux (div a 10) (b * 10) ++ [mod a 10 * b]



    getRomanAprox :: Int -> [(String, Int)] -> Int -> String
    getRomanAprox 0 _ _ = ""
    getRomanAprox a ((b,c):(d,e):xs) f
                        | a == c = b
                        | a >= e-f && a < e = getRomanAprox (e - a) decimalBase f ++ d
                        | a > c && a < e = b ++ getRomanAprox (a-c) decimalBase f
                        | otherwise = getRomanAprox a ((d,e):xs) f


    decToRoman :: [Int] -> String
    decToRoman [] = ""
    decToRoman (x:xs) = getRomanAprox x decimalBase (1 * (10 ^ length xs)) ++ decToRoman xs

    conversorIntToRoman :: Int -> String
    conversorIntToRoman a
                    | a >= 0 = decToRoman $ splitNumber a
                    | otherwise = "-" ++ decToRoman (splitNumber (-a))


    opRoman :: (Int -> Int -> Int) -> (String -> String -> String)
    opRoman f a b  = conversorIntToRoman (conversorRomanToInt a `f` conversorRomanToInt b)

    sumRoman :: String -> String -> String
    sumRoman = opRoman (+)

    restRoman :: String -> String -> String
    restRoman = opRoman (-)

    divRoman :: String -> String -> String
    divRoman = opRoman div

    multRoman :: String -> String -> String
    multRoman = opRoman (*)

    modRoman :: String -> String -> String
    modRoman = opRoman mod


