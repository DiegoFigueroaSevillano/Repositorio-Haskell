module SwapNodes where

data Tree a = Leaf a | Vacio | Node (Tree a) a (Tree a) deriving (Show)

instance Functor Tree where
    fmap f Vacio = Vacio
    fmap f (Leaf x) = Leaf (f x)
    fmap f (Node leftT x rightT) = Node (fmap f leftT) (f x) (fmap f rightT)

initialTree :: Tree Int
initialTree = Leaf 1

profundidad :: Tree a -> Int
profundidad t = getProfundidad t 1

getProfundidad :: Tree a -> Int -> Int
getProfundidad (Leaf _)  x = x
getProfundidad (Node Vacio _ _ ) x = x
getProfundidad Vacio x = 0
getProfundidad (Node leftTree _ _ ) x = getProfundidad leftTree (x+1)

addNodes :: Tree Int -> Int -> Int -> Tree Int
addNodes (Leaf x) a b
                | a < 0 && b < 0 = Node Vacio x Vacio
                | a < 0 && b > 0 = Node Vacio x (Leaf b)
                | a > 0 && b < 0 = Node (Leaf a) x Vacio
                | otherwise = Node (Leaf a) x (Leaf b)
addNodes (Node Vacio x rightT) a b = Node Vacio x (addNodes rightT a b)
addNodes (Node leftT x Vacio) a b = Node (addNodes leftT a b) x Vacio
addNodes (Node (Node Vacio x Vacio) y righT) a b = Node (Node Vacio x Vacio) y (addNodes righT a b)
addNodes (Node leftT y (Node Vacio x Vacio)) a b = Node (addNodes leftT a b) y (Node Vacio x Vacio)
addNodes (Node leftT x rightT) a b = if getMinDepth leftT <= getMinDepth rightT
                                        then Node (addNodes leftT a b) x rightT
                                        else Node leftT x (addNodes rightT a b)

getMinDepth :: Tree a -> Int
getMinDepth (Leaf x) = 0
getMinDepth Vacio = 1
getMinDepth (Node leftT x (Node Vacio y Vacio)) = 1 + getMinDepth leftT
getMinDepth (Node (Node Vacio y Vacio) x righT) = 1 + getMinDepth righT
getMinDepth (Node Vacio x righT) = 1 + getMinDepth righT
getMinDepth (Node leftT x Vacio) = 1 + getMinDepth leftT
getMinDepth (Node leftT x rightT)
                            | profundidad leftT <= profundidad rightT = 1 + getMinDepth leftT
                            | otherwise = 1 + getMinDepth rightT

secondExample :: Tree Int
secondExample = addNodes (addNodes (addNodes (addNodes (addNodes initialTree 2 3) (-1) 4) (-1) 5) (-1) (-1)) (-1) (-1)

thirdExample :: Tree Int
thirdExample = addNodes (addNodes (addNodes (addNodes (addNodes (addNodes (addNodes (addNodes (addNodes (addNodes (addNodes initialTree 2 3) 4 (-1)) 5 (-1)) 6 (-1)) 7 8) (-1) 9) (-1) (-1)) 10 11) (-1) (-1)) (-1) (-1)) (-1) (-1)

swapNodes :: Tree a -> Int -> Int -> Tree a
swapNodes Vacio a b = Vacio
swapNodes (Node (Node Vacio x Vacio) y Vacio) a b = Node Vacio y (Node Vacio x Vacio)
swapNodes (Node Vacio y (Node Vacio z Vacio)) a b = Node (Node Vacio z Vacio) y Vacio
swapNodes (Node (Node Vacio x Vacio) y (Node Vacio z Vacio)) a b = Node (Node Vacio z Vacio) y (Node Vacio x Vacio)
swapNodes (Node leftT x rightT) a b = if b == a then Node (swapNodes rightT a (b+1)) x (swapNodes leftT a (b+1)) else Node (swapNodes leftT a (b+1)) x (swapNodes rightT a (b+1))

inordenTree :: Tree a -> [a]
inordenTree Vacio = []
inordenTree (Leaf x) = [x]
inordenTree (Node Vacio x rightT) = x:inordenTree rightT
inordenTree (Node leftT x Vacio) = inordenTree leftT ++ [x]
inordenTree (Node leftT x rightT) = inordenTree leftT ++ x:inordenTree rightT


aumentarEnUno :: Tree Int -> Tree Int
aumentarEnUno = fmap (+1)

ejemploArbol :: Tree Int
ejemploArbol = Node (Leaf 1) 2 (Node Vacio 3 (Leaf 4))

resultado :: Tree Int
resultado = aumentarEnUno ejemploArbol