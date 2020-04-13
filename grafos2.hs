module Grafos where

type Vertice = Int
type Arista = (Vertice, Vertice)
type Grafo = [(Vertice, [Vertice])]

construirGrafo::Int->Grafo
construirGrafo 1 = [(1,[])]
construirGrafo n = (n,[]):(construirGrafo(n-1))

agregarArista::Grafo->Vertice->Vertice->Grafo
agregarArista [] a b = []
agregarArista ((x,y):(gs)) a b
    | x == a = ((x,b:y):(gs))
    | otherwise = (x,y):(agregarArista (gs) a b)

vertices::Grafo->[Vertice]
vertices [] = []
vertices ((x,y):xs) = x:(vertices xs)

aristas::Grafo->[Arista]
aristas [] = []
aristas ((x,[]):ss) = aristas ss
aristas ((x,(y:ys)):ss) = (x,y):(aristas ((x,ys):ss))

gradoEntrada::Grafo->Vertice->Int
gradoEntrada [] _ = 0
gradoEntrada ((x,y):ss) n
    | elem n y = 1 + (gradoEntrada ss n)
    | otherwise = gradoEntrada ss n

gradoSalida::Grafo->Vertice->Int
gradoSalida [] _ = 0
gradoSalida ((x,y):ss) n
    | (x == n) = length y 
    | otherwise = gradoSalida ss n

grado::Grafo->Vertice->Int
grado a b = ((gradoEntrada a b) + (gradoSalida a b))

--transpuesta::Grafo->Grafo

--------


eliminarVertice::Grafo->Vertice->Grafo
eliminarVertice [] _ = []
eliminarVertice ((a,b):gs) v
    | a == v = eliminarVertice gs v
    | otherwise = (a,b) : (eliminarVertice gs v)

gradoEntradaCero::Grafo->Vertice->Bool
gradoEntradaCero g v = not (gradoEntradaMayorCero g v)

gradoEntradaMayorCero::Grafo->Vertice->Bool
gradoEntradaMayorCero [] _ = False
gradoEntradaMayorCero ((a,b):gs) v
    | elem a b = True
    | otherwise = gradoEntradaMayorCero gs v

primerGradoCero::Grafo->Vertice
primerGradoCero [] = error "Ciclo"
primerGradoCero ((a,b):gs)
    | gradoEntradaCero ((a,b):gs) a = a
    | otherwise = primerGradoCero gs


-- ordenTopologico::Grafo->[Vertice]
-- ordenTopologico [] = []
-- ordenTopologico ((a,b):gs) = primerGradoCero ((a,b):gs) : (ordenTopologico (eliminarVertice  primerGradoCero ((a,b):gs)))
