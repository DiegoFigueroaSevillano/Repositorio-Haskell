module TypeDeclaration where

    -- SINONIMO DE TIPOS 
    type DiaLiteral = String
    type DiaNumeral = Int
    type MesNumeral = Int
    type AnioNumeral = Int

    --CASE OF SIMILAR A SWITCH 
    queDiaEs :: DiaNumeral -> DiaLiteral
    queDiaEs x = case x of
                1 -> "Domingo"
                2 -> "Lunes"
                3 -> "Martes"
                4 -> "Miercoles"
                5 -> "Jueves"
                6 -> "Viernes"
                7 -> "Sabado"
                _ -> error "error en ingresar la fecha"

    diasDelMes :: MesNumeral -> AnioNumeral -> Int
    diasDelMes m a =
                    let
                        esBiciesto = añoBisiesto a
                    in
                        case m of
                            1 -> 31
                            2 -> if esBiciesto then 29 else 28
                            3 -> 31
                            4 -> 30
                            5 -> 31
                            6 -> 30
                            7 -> 31
                            8 -> 30
                            9 -> 31
                            10 -> 30
                            11 -> 31
                            12 -> 30
                            _ -> error "Fecha inexistente"


    añoBisiesto :: AnioNumeral -> Bool
    añoBisiesto a
                | mod a 4 == 0 && mod a 100 /= 0 = True
                | otherwise = False