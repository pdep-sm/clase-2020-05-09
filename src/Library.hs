module Library where
import PdePreludat

type Error = String
type Instruccion = Micro -> Micro
type Programa = [Instruccion]

data Micro = Micro {
    memoria :: [Number],
    acumuladorA :: Number,
    acumuladorB :: Number,
    programCounter :: Number,
    etiqueta :: Error,
    programa :: Programa
} deriving (Show, Eq)

xt8088 = Micro {
    memoria = [],
    acumuladorA = 0,
    acumuladorB = 0,
    etiqueta = "",
    programCounter = 0,
    programa = []
}

incrementarPC :: Instruccion
incrementarPC micro = micro { programCounter = programCounter micro + 1 }

nop :: Instruccion
nop = id

lodv :: Number -> Instruccion
lodv valor micro = micro { 
    acumuladorA = valor
}

swap :: Instruccion
swap micro = micro { 
    acumuladorB = acumuladorA micro,
    acumuladorA = acumuladorB micro
}

add, add' :: Instruccion
add micro = micro {
    acumuladorA = acumuladorA micro + acumuladorB micro,
    acumuladorB = 0
}
add' micro = lodv (acumuladorA micro + acumuladorB micro) micro { acumuladorB = 0 }

division, division' :: Instruccion
division micro 
    | acumuladorB micro == 0 = micro { etiqueta = "DIVISION BY ZERO" }
    | otherwise = incrementarPC micro {
            acumuladorA = acumuladorA micro `div` acumuladorB micro,
            acumuladorB = 0
        }

division' micro@Micro { acumuladorB = 0 } = incrementarPC micro { etiqueta = "DIVISION BY ZERO" }
division' micro = micro {
            acumuladorA = acumuladorA micro `div` acumuladorB micro,
            acumuladorB = 0
        }

str :: Number -> Number -> Instruccion
str direccion valor micro = micro {
    memoria = take (direccion - 1) memo ++ valor : drop direccion memo
} where memo = memoria micro

lod :: Number -> Instruccion
lod direccion micro = micro {
    acumuladorA = memoria micro !! (direccion - 1)
}

ifnz :: Programa -> Instruccion
ifnz prog micro
    | (/=0) . acumuladorA $ micro = ejecutarPrograma prog micro
    | otherwise = micro

ejecutar :: Micro -> Micro
ejecutar micro = ejecutarPrograma (programa micro) micro

ejecutarPrograma prog micro = foldl procesar micro prog 
ejecutarPrograma' = flip foldl procesar

procesar :: Micro -> Instruccion -> Micro
procesar micro instruccion
    | not.null.etiqueta $ micro = micro
    | otherwise = incrementarPC $ instruccion micro

depurar :: Programa -> Programa
depurar = filter (\instruccion -> not . nulo . instruccion $ xt8088)
    where
      nulo micro = all (==0) (acumuladorA micro : acumuladorB micro : memoria micro)

memoriaOrdenada :: Micro -> Bool
memoriaOrdenada micro = estaOrdenada.memoria $ micro
estaOrdenada [] = True
estaOrdenada [_] = True
estaOrdenada (x1:x2:xs) = x1 <= x2 && estaOrdenada (x2:xs)

memoriaOrdenada' micro = all (uncurry (<=)) (zip mem $ tail mem)
    where mem = memoria micro

memoriaOrdenada'' micro = and (zipWith (<=) mem $ tail mem)
    where mem = memoria micro
