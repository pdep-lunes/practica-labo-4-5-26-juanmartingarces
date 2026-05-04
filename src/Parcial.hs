module Parcial where
import Text.Show.Functions()

doble :: Int -> Int
doble = (*2)


data Perros = UnPerrito{
    raza = String,
    jugutesFav = [String],
    tiempo = int,
    energia = int
} deriving (Show)

modificarEnergia :: (Int->Int) -> Perros -> Perros
modificarEnergia unaFuncion unPerro = unPerro{energia = max 0 . unaFuncion . energia $ unPerro }

modificarJuguete :: ([String]->[String]) ->Perros->Perros
modifcarJuguetes otraFuncion unPerro = unPerro{juguetesFav = otraFuncion . juguetesFav $ unPerro}
data Algo = UnAlgo {
    a :: Tipo
}

modificarA :: (Tipo -> Tipo) -> Algo -> Algo
 

jugar :: Perros -> Perros
jugar unPerro = modificarEnergia (subtract 10) unPerro

ladrar :: Int -> Perros -> Perros
ladrar cant unPerro = modificarEnergia (+ (cant/2)) unPerro

regalar :: String -> Perros -> Perros
-- regalar juguetito unPerro = unPerro {jugueteFav = juguetito : jugueteFav unPerro }
regalar juguetito unPerro = modificarJuguete (juguetito :) unPerro

(:) :: a -> [a] -> [a]
(:) :: Str -> [Str] -> [Str]
(juguetito :) :: [Str] -> [Str]

razaExtravagante :: String -> Bool
razaExtravagante "Dalmata" = True
razaExtravagante "Pomerania" = True
razaExtravagante _ = False

diaDeSpa :: Perros -> Perros
diaDeSpa unPerro
    | tiempo unPerro > 50 || razaExtravagante (raza unPerro) = (modificarEnergia (const 100) . regalar "Peine de goma") unPerro
    | otherwise = unPerro

diaDeCampo :: Perros -> Perros
diaDeCampo unPerro = modificarJuguetes (drop 1) unPerro
    --unPerro{jugueteFav = drop 1 juguetesFav}