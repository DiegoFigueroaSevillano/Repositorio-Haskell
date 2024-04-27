module Theme6Exercises where 

    {-
        5.Using the recursive definitions given in this chapter, show how length
        [1,2,3,4,5], and init [1,2,3] are evaluated.
    -}

    myLenght :: [a] -> Int
    myLenght  [] = 0
    myLenght (x:xs) = 1 + myLenght xs

    myDrop :: Int -> [a] -> [a]
    myDrop 0 xs = xs
    myDrop a (x:xs) = myDrop (a-1) xs

    myInit :: [a] -> [a]
    myInit [_] = []
    myInit (x:xs) = x : myInit xs


    {-
        6.Without looking at the definitions from the standard prelude, define the following library functions
        on lists using recursion.
        a.Decide if all logical values in a list are True:
        3
        and :: [Bool] -> Bool
        b.
        Concatenate a list of lists:
        concat :: [[a]] -> [a]
        c.
        Produce a list with n identical elements:
        replicate :: Int -> a -> [a]
        d.
        Select the nth element of a list:
        (!!) :: [a] -> Int -> a
        e.
        Decide if a value is an element of a list:
        elem :: Eq a => a -> [a] -> Bool
        Note: most of these functions are defined in the prelude using other library functions rather than
        using explicit recursion, and are generic functions rather than being specific to the type of lists.
    -}

    myAnd :: [Bool] -> Bool
    myAnd [] = True
    myAnd (False:xs) =  False
    myAnd (x:xs) = myAnd xs

    myConcat :: [[a]] -> [a]
    myConcat [] = []
    myConcat (x : xs) = x ++ myConcat xs

    myReplicate :: Int -> a -> [a]
    myReplicate 0 _ = []
    myReplicate a b = b:myReplicate (a-1) b

    mySelect :: [a] -> Int -> a
    mySelect (x:xs) 0 = x
    mySelect (x:xs) a = mySelect xs (a-1)

    myElem :: Eq a => a -> [a] -> Bool
    myElem _ [] = False
    myElem a (x:xs) 
            | a == x = True
            | otherwise = myElem a xs


    {-
        7.
        Define a recursive function merge :: Ord a => [a] -> [a] -> [a] that merges two sorted
        lists to give a single sorted list. For example:
        > merge [2,5,6] [1,3,4]
        [1,2,3,4,5,6]
        Note: your definition should not use other fu
    -}

    {- REALICE EL EJERCICIO 8 IGUAL EN ESTA PARTE -}

    myMerge :: Ord a => [a] -> [a] -> [a]
    myMerge xs ys = quickSort $ xs ++ ys
        where 
            quickSort :: Ord a => [a] -> [a]
            quickSort [] = []
            quickSort (x:xs) = quickSort [y | y <- xs, y < x] ++ [x] ++ quickSort [y | y <- xs, y >= x]

    
    {-
        9.
        Using the five-step process, construct the library functions that:
        a. calculate the sum of a list of numbers;
        b. take a given number of elements from the start of a list;
        c. select the last element of a non-empty list.
    -}

    mySum :: Num a => [a] -> a
    mySum [] = 0
    mySum (x:xs) = x + mySum xs

    myTake :: Int -> [a] -> [a]
    myTake 0 (x:xs) = []
    myTake a (x:xs) = x:myTake (a-1) xs

    myLast :: [a] -> a
    myLast (x:xs) 
        | length xs == 0 = x
        | otherwise = myLast xs

    
