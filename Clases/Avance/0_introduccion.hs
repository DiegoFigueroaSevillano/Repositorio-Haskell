module Ejemplo where

import Data.Char(ord)
import Data.Binary.Get (Decoder(Fail))

nextNum :: Num a => a -> a
nextNum n = n + 1

square :: Num a => a -> a
square n = n*n

suma :: Int -> Int -> String
suma a b = show (a + b)

cadena :: String
cadena = "Hola mundo"

--comentario 
{-
Comentario 
-}

--TIPO DE DATO BOOLEANO 
esPar :: Int -> Bool
esPar = even

--TIPO DE DATO INT y OPERACIONES ARMITMETICAS 
-- Int para numeros pequeños 
-- Integer para numeros garndes 

-- / Fractional
-- div para divisiones enteras 
-- mod Para calcular el modulo 

-- para elevar se usa el v al revez 

suma' :: Float -> Float -> Float
suma' a b = a + b

unChar :: Char
unChar = 'D'

getOrd :: Char -> Int
getOrd a = ord unChar

--TUPLAS 
-- UNA COLECCION DEVALORES INMUTABLES Y ORDENADOS 

tupla :: Char -> (Char, Int)
tupla a = (a, ord a)

sumTupla :: (Int, Int, Char) -> Int
sumTupla (a, b, c) = a + b

sumLista :: [Int] -> Int
sumLista = sum --Se agrega xs a la lista para decir QUE ESTAMOS AGARRANDO TODA LA LISTA 

filtrar :: [Int] -> [Int]
filtrar = filter esPar

miSplit :: Int -> [Int] -> ([Int],[Int])
miSplit = splitAt

myAbs :: Int -> Int
myAbs n = if n >= 0 then n else -n

--Guarded Equiation 
validPositiveNumber :: Int -> Int
validPositiveNumber n
  | n > 0 = 1
  | n == 0 = 0
  | otherwise = -1

--Pattern Matching 
negar :: Bool -> Bool
negar True = False 
negar False = True 

esUno :: Int -> Bool 
esUno 1 = True
esUno _ = False --EL underscore significa cualquier cosa 

--Lambda Function 
add :: Int -> (Int -> Int) 
add = \x -> (\y -> x + y) -- El lambda es otra forma dep edir argumentos 

myFunction :: [(Int -> Int -> Int)]
myFunction = [(+), (-), (*), div]

getFunction :: Char -> (Int -> Int -> Int)
getFunction e 
          | e == '+' = (+)
          | e == '-' = (-)
          | e == '*' = (*)
          | e == '/' = div
          | otherwise = undefined


--Carry
-- QUE ES CURRIED FUNCTION 
mulThree :: Int -> (Int -> (Int -> Int)) 
mulThree x y z = x * y * z

-- Es l mismo q hacer (((mulThree x) y) z)

--LISTAS POR COMPRENSION 
-- [(x, y) | x <- [1..5], y <- [6..10]]

-- Para concatenar dos listas de usa el ++
--MI QUICKSORT 
-- Para agarrar la lita y su valor podemos hacer lo siguiente (x:xs) me da el primer valor y la lista 
quicksort :: [Int] -> [Int]
quicksort [] = []
quicksort xs = quicksort (filter (\x -> x < head xs) xs) ++ [head xs] ++  quicksort (filter (\x -> x > head xs) xs)


-- PODEMOS USAR EL WHERE PARA DECLARAR NUESTRAS FUNCIONES PRIVADAS DENTRO DE UNA FUNCION 


esPrimo :: Int -> Bool
esPrimo a = False