module Arboles2 where

data ExpArit = K Int| N ExpArit | (:+) ExpArit ExpArit| (:*) ExpArit ExpArit
    deriving(Show)



espejo::ExpArit ->ExpArit
espejo (K v1) = K v1
espejo (N a1) = N (espejo a1)
espejo ((:+) a1 a2) = (:+) (espejo a2) (espejo a1)
espejo ((:*) a1 a2) = (:*) (espejo a2) (espejo a1)