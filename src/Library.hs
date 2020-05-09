module Library where
import PdePreludat

type Error = String
type Instruccion = Micro -> Micro
type Programa = Instruccion

data Micro = Micro {
    memoria :: [Number],
    acumuladorA :: Number,
    acumuladorB :: Number,
    programCounter :: Number,
    etiqueta :: Error
} deriving (Show, Eq)

xt8088 = Micro {
    memoria = [],
    acumuladorA = 0,
    acumuladorB = 0,
    etiqueta = "",
    programCounter = 0
}

incrementarPC :: Instruccion
incrementarPC micro = micro { programCounter = programCounter micro + 1 }

nop :: Instruccion
nop = incrementarPC

lodv :: Number -> Instruccion
lodv valor micro = incrementarPC micro { 
    acumuladorA = valor
}

swap :: Instruccion
swap micro = incrementarPC micro { 
    acumuladorB = acumuladorA micro,
    acumuladorA = acumuladorB micro
}

add, add' :: Instruccion
add micro = incrementarPC micro {
    acumuladorA = acumuladorA micro + acumuladorB micro,
    acumuladorB = 0
}
add' micro = lodv (acumuladorA micro + acumuladorB micro) micro { acumuladorB = 0 }

division, division' :: Instruccion
division micro 
    | acumuladorB micro == 0 = incrementarPC micro { etiqueta = "DIVISION BY ZERO" }
    | otherwise = incrementarPC micro {
            acumuladorA = acumuladorA micro `div` acumuladorB micro,
            acumuladorB = 0
        }

division' micro@Micro { acumuladorB = 0 } = incrementarPC micro { etiqueta = "DIVISION BY ZERO" }
division' micro = incrementarPC micro {
            acumuladorA = acumuladorA micro `div` acumuladorB micro,
            acumuladorB = 0
        }

str :: Number -> Number -> Instruccion
str direccion valor micro = incrementarPC micro {
    memoria = take (direccion - 1) memo ++ valor : drop direccion memo
} where memo = memoria micro

lod :: Number -> Instruccion
lod direccion micro = incrementarPC micro {
    acumuladorA = memoria micro !! (direccion - 1)
}
