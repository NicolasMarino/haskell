module Grafos where

type Vertice = Int
    
type Arista = (Vertice, Vertice)
    
type Grafo = [(Vertice, [Vertice])]
    
    
--Construir Grafo
    
construirGrafo::Int->Grafo
construirGrafo 0 = []
construirGrafo n = (n,[]): construirGrafo (n-1)
    
--Agregar Arista
 
agregarArista::Grafo->Vertice->Vertice->Grafo
agregarArista g a b = []
agregarArista ((vertice,xs):ys) a b 
        | vertice == a = (vertice,(b:xs)):ys
        | otherwise = (vertice,xs): agregarArista ys a b

vertices::Grafo->[Vertice]
vertices [] = []
  
vertices ((vertice,xs):ys) = (vertice : vertices ys)

aristas::Grafo->[Arista]
aristas [] = []
aristas ((vertice,[]):ys) = aristas ys
aristas ((vertice,(x:xs)):ys) = (vertice,x) : aristas ((vertice,xs):ys)

--Aristas que se conectan desde cualquier vertice al vertice que estamos contando
gradoEntrada::Grafo->Vertice->Int
gradoEntrada [] verticeNuevo = 0
gradoEntrada ((vertice,(x:xs)):ys) verticeNuevo
             | elem verticeNuevo xs = 1 + gradoEntrada ys verticeNuevo
             | not(elem verticeNuevo xs) = gradoEntrada ys verticeNuevo
-- --Aristas que se conectan a un det vertice con cualquier otro
-- gradoSalida::Grafo->Vertice->Int
-- gradoSalida

-- --Cantidad de aristas que se encuentran conectando a un vertice
-- gradoIncidencia::Grafo->Vertice->Int
-- gradoIncidencia

-- ordenTopologico::Grafo->[Vertice]
