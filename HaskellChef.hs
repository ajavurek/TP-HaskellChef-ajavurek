module HaskellChef where
import Text.Show.Functions()

data Participante = UnParticipante {
    nombre :: String,
    trucosDeCocina :: [Truco],
    especialidad :: Plato
} deriving (Show)

data Plato = UnPlato {
    dificultad :: Float,  -- Entre 0 y 10
    componentes :: [Componente]
} deriving (Show, Eq)

type Truco = Plato -> Plato
type Componente = (Ingrediente, Gramos)
type Ingrediente = String
type Gramos = Float

-- PARTE A:

modificarComponentes :: ([Componente] -> [Componente]) -> Plato -> Plato
modificarComponentes unaF unPlato = unPlato { componentes = unaF (componentes unPlato)}

endulzar :: Gramos -> Truco
endulzar cantGramos = modificarComponentes (("Azúcar", cantGramos) :)

salar :: Gramos -> Truco
salar cantGramos = modificarComponentes (("Sal", cantGramos) :)

darSabor :: Gramos -> Gramos ->Truco
darSabor gramosSal gramosAzucar = endulzar gramosAzucar . salar gramosSal

duplicarPorcion :: Truco
duplicarPorcion = modificarComponentes (map duplicarPeso)

duplicarPeso :: Componente -> Componente
duplicarPeso (ing, gr) = (ing, gr *2)

simplificar :: Truco
simplificar unPlato
    | esComplejo unPlato = modificarComponentes (filter tienePesoAceptable) (unPlato { dificultad = 5})
    | otherwise = unPlato

tienePesoAceptable :: Componente -> Bool
tienePesoAceptable = (>= 10) . snd

esComplejo :: Plato -> Bool
esComplejo unPlato = ((>5) . length . componentes) unPlato && ((>7) . dificultad) unPlato

esVegano :: Plato -> Bool
esVegano = not . any esProductoNoVegano . ingredientesDeUnPlato

ingredientesDeUnPlato :: Plato -> [Ingrediente]
ingredientesDeUnPlato unPlato = map fst (componentes unPlato)

esProductoNoVegano :: Ingrediente -> Bool
esProductoNoVegano ingrediente = ingrediente `elem` ["Carne", "Huevos", "Lácteos"]

esSinTacc :: Plato -> Bool
esSinTacc = not . elem "Harina" . ingredientesDeUnPlato

noAptoHipertension :: Plato -> Bool
noAptoHipertension = any esSalado . componentes

esSalado :: Componente -> Bool
esSalado (ing, gr) = ing == "Sal" && gr > 2

-- PARTE B:

pepeRonccino :: Participante
pepeRonccino = UnParticipante {
    nombre = "Pepe Ronccino",
    trucosDeCocina = [darSabor 2 5, duplicarPorcion, simplificar],
    especialidad = platoDePepe 
}

platoDePepe :: Plato
platoDePepe = UnPlato {
        dificultad = 8,
        componentes = [("Sal", 3), ("Langostinos", 200), ("Rucula", 50), ("Lechuga", 50), ("Palta", 100), ("Tomate", 80), ("Jugo de Limón", 15)]
    }

--PARTE C:

esMejorQue :: Plato -> Plato -> Bool
esMejorQue plato1 plato2 = (dificultad plato1 > dificultad plato2) && (pesoTotal plato1 < pesoTotal plato2)

pesoTotal :: Plato -> Gramos
pesoTotal = sum . map snd . componentes

cocinar :: Participante -> Plato
cocinar unParticipante = foldl aplicarTruco (especialidad unParticipante) (trucosDeCocina unParticipante)

aplicarTruco :: Plato -> Truco -> Plato
aplicarTruco unPlato unTruco = unTruco unPlato

participanteEstrella :: [Participante] -> Participante
participanteEstrella [] = []
participanteEstrella [unP1] = unP1
participanteEstrella (unP1:unP2:restoP)
    | esMejorQue (cocinar unP1) (cocinar unP2) = participanteEstrella (unP1:restoP) 
    | otherwise = participanteEstrella (unP2:restoP)

-- PARTE D: