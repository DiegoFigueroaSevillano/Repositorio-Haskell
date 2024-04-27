module Tipos where

    data Boolean = False | True

    type Pos = (Int, Int)
    data Move = North | South | East | West

    move :: Move -> Pos -> Pos
    move North (x,y) = (x,y+1)
    move South (x,y) = (x,y+1)
    move East (x,y) = (x,y+1)
    move West (x,y) = (x,y+1)

    data Shape = Circle Float | Rect Float Float | Cua Float Float deriving (Show)

    square :: Float -> Shape
    square n = Rect n n

    area :: Shape -> Float
    area (Circle r) = pi * r**2
    area (Rect x y) = x * y

    {-
    data Maybe a = Nothing | Just a
    -}

    safeDiv :: Int -> Int -> Maybe Int
    safeDiv _ 0 = Nothing
    safeDiv m n = Just (div m n)

    getValue :: Maybe Int -> Int
    getValue Nothing = 0
    getValue (Just a) = a



    -- TIPOS DE DATOS 

    data Nat = Zero | Succ Nat deriving (Show)

    natToNum :: Nat -> Int
    natToNum Zero = 0
    natToNum (Succ n) = 1 + natToNum n

    numToNat :: Int -> Nat
    numToNat 0 = Zero
    numToNat n = Succ (numToNat (n-1))

    suma :: Nat -> Nat -> Nat
    suma Zero n = n
    suma (Succ m) n = Succ (suma m n)

    -- ARBOLES 

    data Tree a = Leaf a | Vacio | Node (Tree a) a (Tree a) deriving (Show)

    arbol :: Tree Int
    arbol = Node (Node (Node Vacio 2 (Leaf 7)) 23 (Leaf 38)) 45 (Node (Node (Leaf 48) 52 Vacio) 65 (Leaf 96))

    -- data List a = Vacio | Siguiente a (List a) deriving (Show)

    -- myList :: List String 
    -- myList = Siguiente "H" (Siguiente "o" (Siguiente "l" (Siguiente "a" Vacio)))

    -- myListLength :: List a -> Int
    -- myListLength Vacio = 0 
    -- myListLength (Siguiente _ xs) = 1 + myListLength xs 

    -- myListToList :: List a -> [a]
    -- myListToList Vacio = []
    -- myListToList (Siguiente a xs) = a:myListToList xs


    addNode :: Ord a => Tree a -> a -> Tree a
    addNode (Leaf x) y = if x > y then Node (Leaf y) x Vacio else Node Vacio x (Leaf y)
    addNode (Node Vacio x Vacio) y = if x > y then Node (Leaf y) x Vacio else Node Vacio x (Leaf y)
    addNode (Node Vacio x rightT) y = if x > y then Node (Leaf y) x rightT else addNode rightT y
    addNode (Node leftT x Vacio) y = if x < y then Node leftT x (Leaf y) else addNode leftT y
    addNode (Node leftT x rightT) y = if x > y then Node (addNode leftT y ) x rightT else Node leftT x (addNode rightT y)

    inordenTree :: Tree a -> [a]
    inordenTree (Leaf x) = [x]
    inordenTree (Node Vacio x rightT) = x:inordenTree rightT
    inordenTree (Node leftT x Vacio) = inordenTree leftT ++ [x]
    inordenTree (Node leftT x rightT) = inordenTree leftT ++ x:inordenTree rightT

    preordenTree :: Tree a -> [a]
    preordenTree (Leaf x) = [x]
    preordenTree (Node leftT x Vacio) = x:preordenTree leftT
    preordenTree (Node Vacio x rightT) = x:preordenTree rightT
    preordenTree (Node leftT x rightT) = x:preordenTree leftT ++ preordenTree rightT

    postordenTree :: Tree  a -> [a]
    postordenTree (Leaf x) = [x]
    postordenTree (Node leftT x Vacio) = postordenTree leftT ++ [x]
    postordenTree (Node Vacio x rightT) = postordenTree rightT ++ [x]
    postordenTree (Node leftT x rightT) = postordenTree leftT ++ postordenTree rightT ++ [x]