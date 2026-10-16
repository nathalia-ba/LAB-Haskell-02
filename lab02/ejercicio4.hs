procesarLecturas :: (Double -> Double) -> [Double] -> [Double]
procesarLecturas funcion datos = map funcion datos

celsiusAFahrenheit :: Double -> Double
celsiusAFahrenheit c = c * 9 / 5 + 32

crearFiltro :: (a -> Bool) -> ([a] -> [a])
crearFiltro condicion = filter condicion

enRango :: Int -> Int -> (Int -> Bool)
enRango minimo maximo = \x -> x >= minimo && x <= maximo

potenciador :: Int -> (Int -> Int)
potenciador potencia = \x -> x ^ potencia
