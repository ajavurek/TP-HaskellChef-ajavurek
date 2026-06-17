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
type Gramos = Int

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
    | esComplejo unPlato = modificarComponentes (filter tienePesoAceptable) . modificarDificultad 5 $ unPlato
    | otherwise = unPlato

modificarDificultad :: Float -> Plato -> Plato
modificarDificultad nuevaDificultad unPlato = unPlato { dificultad = nuevaDificultad }

tienePesoAceptable :: Componente -> Bool
tienePesoAceptable = (>= 10) . snd

esComplejo :: Plato -> Bool
esComplejo unPlato = ((>5) . length . componentes) unPlato && ((>7) . dificultad) unPlato

contiene :: [Ingrediente] -> Plato -> Bool
contiene unosIngredientes unPlato = any (`elem` unosIngredientes) . ingredientesDeUnPlato $ unPlato

ingredientesDeUnPlato :: Plato -> [Ingrediente]
ingredientesDeUnPlato unPlato = map fst (componentes unPlato)

esVegano :: Plato -> Bool
esVegano = not . contiene ["Carne", "Huevos", "Lácteos"]

esSinTacc :: Plato -> Bool
esSinTacc = not . contiene ["Harina"]

noAptoHipertension :: Plato -> Bool
noAptoHipertension = any esSalado. componentes

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
participanteEstrella [] = error "Lista de Participantes vacía"
participanteEstrella [unP] = unP
participanteEstrella (unP:restoP)
    | esMejorQue (cocinar unP) (cocinar (participanteEstrella restoP)) = unP
    | otherwise = participanteEstrella restoP

-- PARTE D:

armarComponente :: Int -> Componente
armarComponente unNum = ("Ingrediente" ++ show unNum, unNum)

ingredientesInf :: [Componente]
ingredientesInf = map armarComponente [1..]

platinium :: Plato
platinium = UnPlato{
    dificultad = 10,
    componentes = ingredientesInf
}
