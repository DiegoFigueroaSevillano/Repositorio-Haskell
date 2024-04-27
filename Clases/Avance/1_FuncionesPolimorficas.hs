module FuncionesPolimorficas where

    suma :: Int -> Int -> Int
    suma a b = a + b

    sumaPolimorfica :: Num a => a -> a -> a
    sumaPolimorfica a b = a + b 

    fact :: Int -> Int 
    fact 0 = 1
    fact n = n * fact (n-1)

    -- La complejidad esta en aplicar la recursividad en listas 

    myLenght :: [a] -> Int
    myLenght [] = 0
    myLenght (_:xs) = 1 + myLenght xs

    insertVal :: Ord a => a -> [a] -> [a]
    insertVal x [] = [x]
    insertVal x (y:ys) 
                | x <= y = x:y:ys
                | otherwise =  y:insertVal x ys

    
    myZip :: [a] -> [b] -> [(a,b)]
    myZip [] _ = []
    myZip _ [] = []
    myZip (x:xs) (y:ys) = (x,y):myZip xs ys

    -- OTRA FORMA DE CONCATENAR LISTAS ES APLICANDO : 