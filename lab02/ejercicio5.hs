import Data.List (sortOn)

type Estado = (Int, Int)
type Nodo = Estado
type Costo = Int

capA, capB :: Int
capA = 5
capB = 3

transferirAB :: Estado -> Estado
transferirAB (a, b) =
    let cantidad = min a (capB - b)
    in (a - cantidad, b + cantidad)

transferirBA :: Estado -> Estado
transferirBA (a, b) =
    let cantidad = min b (capA - a)
    in (a + cantidad, b - cantidad)

vecinos :: Estado -> [(Estado, Costo)]
vecinos (a, b) =
    [ ((capA, b), 1)
    , ((a, capB), 1)
    , ((0, b), 1)
    , ((a, 0), 1)
    , (transferirAB (a, b), 1)
    , (transferirBA (a, b), 1)
    ]

heuristica :: Estado -> Costo
heuristica _ = 0

aStar :: (Nodo -> [(Nodo, Costo)])
      -> (Nodo -> Costo)
      -> Nodo
      -> Nodo
      -> [(Nodo, Costo)]
aStar sucesores heuristica inicio meta =
    buscar [(inicio, 0, [(inicio, 0)])] []
  where
    buscar [] _ = []

    buscar ((nodo, costo, camino):cola) visitados
        | nodo == meta = camino
        | nodo `elem` visitados = buscar cola visitados
        | otherwise =
            let nuevos =
                    [ (v, costo + c, camino ++ [(v, costo + c)])
                    | (v, c) <- sucesores nodo
                    , v /= nodo
                    , v `notElem` visitados
                    , v `notElem` map fst camino
                    ]

                ordenados =
                    sortOn (\(v, g, _) -> g + heuristica v)
                    (cola ++ nuevos)

            in buscar ordenados (nodo : visitados)

solucion :: [(Estado, Costo)]
solucion = aStar vecinos heuristica (0, 0) (4, 0)
