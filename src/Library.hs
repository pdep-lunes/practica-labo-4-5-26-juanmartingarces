module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero
data Perros = UnPerrito{
    raza :: String,
    juguetesFav :: [String],
    tiempo :: Number,
    energia :: Number
} deriving (Show)

modificarEnergia :: (Number->Number) -> Perros -> Perros
modificarEnergia unaFuncion unPerro = unPerro{energia = max 0 . unaFuncion . energia $ unPerro }

modificarJuguete :: ([String]->[String]) ->Perros->Perros
modificarJuguete otraFuncion unPerro = unPerro{juguetesFav = otraFuncion . juguetesFav $ unPerro}

jugar :: Perros -> Perros
jugar unPerro = modificarEnergia (subtract 10) unPerro

ladrar :: Number -> Perros -> Perros
ladrar cant unPerro = modificarEnergia (+ (cant/2)) unPerro

regalar :: String -> Perros -> Perros
regalar juguetito unPerro = modificarJuguete (juguetito :) unPerro

razaExtravagante :: String -> Bool
razaExtravagante "Dalmata" = True
razaExtravagante "Pomerania" = True
razaExtravagante _ = False

diaDeSpa :: Perros -> Perros
diaDeSpa unPerro
    | tiempo unPerro > 50 || razaExtravagante (raza unPerro) = (modificarEnergia (const 100) . regalar "Peine de goma") unPerro
    | otherwise = unPerro

diaDeCampo :: Perros -> Perros
diaDeCampo unPerro = modificarJuguete (drop 1) unPerro
