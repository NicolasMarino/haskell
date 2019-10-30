module Arboles where

data (Bintree a) = Empty| Node a (Bintree a) (Bintree a)
    deriving(Show)

a::Bintree Int
a = Empty
a1 = Node 1 a a
a2 = Node 2 a1 a
a3 = Node 3 a2 a1
a4 = Node 4 a2 a3

cantidadNodos::Bintree a->Int
cantidadNodos (Empty) = 0
cantidadNodos (Node v1 a1 a2) = 1 + cantidadNodos (a1) + cantidadNodos (a2)

