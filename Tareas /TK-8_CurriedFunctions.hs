module CurriedFunctions where

    multNormal :: Int -> Int -> Int
    multNormal a b = a * b

    -- El currying es la transformacion de una funcion con multiples argumentos en 
    -- una secuencia de funciones de un solo argumento

    -- Nos permite invocar una funcion con menos parametros 

    curriedMult :: Int -> (Int -> Int)
    curriedMult a b = a * b

    -- Para sea mas facil de entender podemos ver la misma funcion en JAVASCRIPT 

    {-
        const curriedMultiply = function(a) {
            return function(b) {
                return a * b;
            }
        }
    -}

--BENEFICIOS
-- Uno de los beneficios de el currying es que podemos invocar una funcion con menos parametros inicialmente
-- Y podemos utilizarlo mas adelane.

--EJEMPLOS 

-- Metodo para mostrar mensaje
    showMessage :: String -> (String -> (String -> String))
    showMessage a b c = a ++ b ++ c

-- Bienvenido Estudiante
    bienvenidoEstudiante :: (String -> String)
    bienvenidoEstudiante = showMessage "Bienvenido " "estudiante: "

-- Mensaje de error 
    mensajeDeError :: (String -> (String -> String))
    mensajeDeError = showMessage "Error "


    mulThree :: Double -> (Double -> (Double -> Double))
    mulThree x y z = x * y * z

    cuentaDeMesa :: Double -> Double
    cuentaDeMesa = mulThree 1.1 1.05