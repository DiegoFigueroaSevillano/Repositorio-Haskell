module Theme3Exercises where 

    {-
    What are the types of the following functions?
    second xs = head (tail xs)
    swap (x,y) = (y,x)
    pair x y = (x,y)
    double x = x*2
    palindrome xs = reverse xs == xs
    twice f x = f (f x)
    Hint: take care to include the necessary class constraints in the types if the functions are defined
    using overloaded operators.
    -}

    -- Lista - Elemento
    second :: [a] -> a
    second xs = head $ tail xs

    -- Tupla - Tupla 
    mySwap :: (a, a) -> (a, a)
    mySwap (x, y) = (y, x)

    -- Pide dos elementos y devuelve una tupla 
    pair :: a -> a -> (a, a)
    pair a b = (a, b)

    -- Pide un elemento del tipo Num y devuelve otro numero 
    double :: Num a => a -> a
    double a = a * 2

    -- Pide un elemento que se pueda comparar y devuelve un booleano || En este caso el elemento es una lista
    palindrome :: Eq a => [a] -> Bool
    palindrome xs = reverse xs == xs

    -- Pide una funcion cualquiera y un elemento y retorna la funcion aplicada al elemento dos veces 
    twice :: (a -> a) -> a -> a
    twice f x = f (f x)


    {-
        Why is it not feasible in general for function types to be instances of the Eq class? When is it
        feasible? Hint: two functions of the same type are equal if they always return equal results for
        equal arguments.
    -}
    -- R: No es muy factible ya que comparar dos funciones podria ser complejo, ya que aunque cumplan la misma tarea
    -- estas pueden ser totalmende distintas en su construccion. 
    -- Sin embargo es factible cuando nuestro objetivo si es comparar dos funciones no complejas 

    
