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

zara :: Perros 
zara = UnPerrito "Dalmata" ["Pelota", "mantita"] 90 80

data Guarderia = UnaGuarderia {
    nombre :: String,
    actividades :: [(Perros -> Perros, Number)]
} deriving (Show)

guarderiaPdePerritos :: Guarderia
guarderiaPdePerritos = UnaGuarderia "PdePerritos" [(jugar, 30), (ladrar 18, 20), (regalar "pelota", 0), (diaDeSpa, 120), (diaDeCampo, 720)]

tiempoTotal :: Guarderia -> Number
tiempoTotal unaGuarderia = sum . map snd . actividades $ unaGuarderia

puedeEstar:: Guarderia -> Perros -> Bool
puedeEstar unaGuarderia unPerro = tiempoTotal unaGuarderia < tiempo unPerro

perrosResponsables :: Perros -> Bool
perrosResponsables unPerro = (> 3) . length . juguetesFav . diaDeCampo $ unPerro

realizarRutina :: Guarderia -> Perros -> Perros
realizarRutina unaGuarderia unPerro
    | puedeEstar unaGuarderia unPerro = ejecutarRutina unaGuarderia unPerro
    | otherwise = unPerro

ejecutarRutina :: Guarderia -> Perros -> Perros
ejecutarRutina unaGuarderia = foldr (.) id (map fst . actividades $ unaGuarderia)

evaluarCansados :: Guarderia -> Perros -> Bool
evaluarCansados unaGuarderia unPerro = 
    puedeEstar unaGuarderia unPerro && energia (realizarRutina unaGuarderia unPerro) < 5

perrosCansados :: Guarderia -> [Perros] -> [Perros]
perrosCansados unaGuarderia losPerros = filter (evaluarCansados unaGuarderia) losPerros