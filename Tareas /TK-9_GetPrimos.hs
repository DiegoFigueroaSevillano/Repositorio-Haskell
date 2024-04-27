module GetPrimos where

    esPrimo :: Int -> Bool
    esPrimo 1 = False
    esPrimo a = length (filter (\x -> rem a x == 0) list) == 1
        where
            list :: [Int]
            list = [2..a]

    getPrimos :: Int -> [Int]
    getPrimos a = filter esPrimo [1..a]


    -- CORRECCION DEL DOCENTE
    -- VISUALIZAR LA FUNCION GO 

