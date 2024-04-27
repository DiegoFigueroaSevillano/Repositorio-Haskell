module Theme7Exercises where

    -- 7.

    
    unfold :: (t -> Bool) -> (t -> a) -> (t -> t) -> t -> [a]
    unfold p h t x
            | p x = []
            | otherwise = h x:unfold p h t (t x)


    int2bin :: Int -> [Int]
    int2bin = unfold (== 0) (`mod` 2) (`div` 2)

    type Bit = Int
    chop8 :: [Bit] -> [[Bit]]
    chop8 [] = []
    chop8 bits = take 8 bits : chop8 (drop 8 bits)

    chop8' :: Eq a => [a] -> [[a]]
    chop8' = unfold ( == []) (take 8) (drop 8)

    -- EL METODO MAP NO SE PUEDE REPLICAR USANDO EL UNFOLD
    -- PORQUE EL METODO UNFULD AGARRA UN DATO GENERICO Y DEVUELVE UNA LISTA DE ESE DATO Y EL MAP NECESITA UNA LISTA PARA ITERAR
    -- map' :: Eq a => [a] -> [a]
    -- map' = concat . unfold ( == [] ) () tail

    iterate' :: Eq a => a -> (a -> a) -> [a]
    iterate' a f = unfold (const False) id f a



    --9.

    altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
    altMap _ _ [] = []
    altMap fx fy (x:y:xs) = fx x: fy y:altMap fx fy xs
    altMap fx _ [x] = [fx x]
