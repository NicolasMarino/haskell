module Dijkstra where
--Nicolás Marino 231142
type Vertice = Int
type Peso = Int
type VerticeConPeso = (Vertice,Peso)

type GrafoP = [(Vertice,[VerticeConPeso])]

type Etiquetas = [(Vertice, Peso)]
type Visitados = [Vertice]

inf = maxBound::Int

-- Ejemplo de la letra
ejemplo1 :: GrafoP
ejemplo1 = [(1, [(2,2), (3,1)]), (2, [(4,1)]), (3, [(4,3), (5,4)]), (4, [(6,2)]), (5, [(6,2)]), (6, [])]

-- Otro ejemplo
ejemplo2 :: GrafoP
ejemplo2 = [(1, [(2,3), (4,5), (3,1), (5,0)]), (2, [(4,1), (3,1), (1,1)]), (3, [(4,2)]), (4, []), (5, [(4,1)])]

-- ******************************************* FUNCIONES A IMPLEMENTAR *******************************************

-- Ejercicio 1
vertices :: GrafoP -> [Vertice]
vertices [] = []
vertices ((vertice,verticePeso):sig) = vertice : vertices sig

-- Ejercicio 2
crearListaEtiquetasSinPeso :: [Vertice] -> Etiquetas
crearListaEtiquetasSinPeso [] = []
crearListaEtiquetasSinPeso (x:xs) = (x,inf) : crearListaEtiquetasSinPeso xs

-- Ejercicio 3 
actualizarPeso :: Etiquetas -> Vertice -> Peso -> Etiquetas
actualizarPeso [] _ _ = []
actualizarPeso ((ve,peso):xs) v p
                | ve == v &&  peso > p = (ve,p) : actualizarPeso xs v p
                | otherwise = (ve,peso) : actualizarPeso xs v p

-- Ejercicio 4 
costoAsignacion :: Etiquetas -> Vertice -> Int
costoAsignacion [] _ = error"No se encuentra ese vertice"
costoAsignacion ((ve,peso):xs) v
                | ve == v = peso
                | otherwise = costoAsignacion xs v
       
-- Ejercicio 5
verticeConAdyacentes :: GrafoP -> Vertice -> (Vertice,[VerticeConPeso])
verticeConAdyacentes [] _ = error"No se encuentra ese vertice"
verticeConAdyacentes ((ve,peso):xs) v
                    | ve == v = (ve,peso)
                    | otherwise = verticeConAdyacentes xs v

-- Ejercicio 6
actualizarAsignacion :: (Vertice, [VerticeConPeso]) ->  Etiquetas -> Etiquetas 
actualizarAsignacion (v, []) etiq = etiq
actualizarAsignacion (v, (vp,pesop):ex) etiq = actualizarAsignacion (v, ex) (actualizarPeso etiq vp (pesop + (costoAsignacion etiq v)))

-- Ejercicio 7
pesoDeUnVertice :: [VerticeConPeso] -> Vertice -> Int
pesoDeUnVertice [] _ = error "No se encuentra ese vertice"
pesoDeUnVertice ((ve,peso):xs) v
                | ve == v = peso    
                | otherwise = pesoDeUnVertice xs v

-- Ejercicio 8
perteneceVertice :: [VerticeConPeso] -> Vertice -> Bool
perteneceVertice [] _ = False
perteneceVertice ((ve,peso):xs) v
                | ve == v = True
                | otherwise = perteneceVertice xs v


-- Ejercicio 9
verticeMenorCostoNoVisitado :: Etiquetas -> Visitados -> Vertice
verticeMenorCostoNoVisitado etiquetas visitados = fst (obtenerEtiquetaMenorPeso(obtenerEtiquetasNoVisitadas etiquetas visitados))

obtenerEtiquetaMenorPeso :: Etiquetas -> (Vertice,Peso)
obtenerEtiquetaMenorPeso [(ve,peso)] = (ve,peso)
obtenerEtiquetaMenorPeso ((ve,peso):(ve2,peso2):xs)
                        | peso < peso2 = obtenerEtiquetaMenorPeso ((ve,peso):xs)
                        | otherwise = obtenerEtiquetaMenorPeso ((ve2,peso2):xs)

obtenerEtiquetasNoVisitadas :: Etiquetas -> Visitados -> Etiquetas
obtenerEtiquetasNoVisitadas [] _ = []
obtenerEtiquetasNoVisitadas ((ve,peso):xs) visitados  
                        | (elem ve visitados) == False = (ve,peso) : obtenerEtiquetasNoVisitadas xs visitados
                        | otherwise = obtenerEtiquetasNoVisitadas xs visitados


-- ************************************************** FIN TAREA **************************************************

cmc :: GrafoP -> Visitados -> Etiquetas -> Vertice -> Vertice -> Peso
cmc g vs es actual destino
    | length (vertices g) == (length vs) = costoAsignacion es destino
    | otherwise = cmc g (actual:vs) (asignacionActualizada) (verticeMenorCostoNoVisitado asignacionActualizada (actual:vs)) destino
        where asignacionActualizada = actualizarAsignacion (verticeConAdyacentes g actual) es

menorCosto :: GrafoP -> Vertice -> Vertice -> Peso
menorCosto g origen destino = cmc g [] (actualizarPeso (crearListaEtiquetasSinPeso (vertices g)) origen 0) origen destino

-- Pruebas de los ejemplos
pruebaEjemplo1 :: Int
pruebaEjemplo1 = menorCosto ejemplo1 1 6 -- resultado deberia ser 5.

pruebaEjemplo2 :: Int
pruebaEjemplo2 = menorCosto ejemplo2 1 4 -- resultado deberia ser 1.