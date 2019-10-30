module TareaUno where

andor::Bool->Bool->Bool->Bool
andor a b c = (a && b) || c

orand::Bool->Bool->Bool->Bool
orand a b c = (a || b) && c

fbool::Bool->Bool->Bool->Bool
fbool a b c = (a || b) && (a || c)

condicional::Bool->Bool->Bool
condicional True False = False
condicional _ _ = True

con2::Bool->Bool->Bool->Bool
con2 a b c = condicional (not(not(a && not(b)))) (not(c))

--2.1 Enteros

divisibleEntre3::Int->Bool
divisibleEntre3 n
                | n == 0 = True
                | n < 0 = False
                | n > 0 = divisibleEntre3 (n-3)
                
multiplicado::Int->Int->Int
multiplicado n m 
            | m == 0 = 0
            | m > 0 = n + multiplicado n (m-1)

filtradoFuncion::(Int->Int)->(Int->Bool)->Int->Int
filtradoFuncion f p 0 = 0
filtradoFuncion f p n
                | p(f n) = n + filtradoFuncion f p (n-1)
                | otherwise = filtradoFuncion f p (n-1)

sum1::Int->Int
sum1 n 
    | n == 1 = 1
    | n > 1 = ((3*n)-2) + sum1 (n-1)

dosala::Int->Int
dosala n
        | n == 0 = 1
        | n > 0 = 2 * dosala (n-1)

sum2::Int->Int
sum2 n 
    | n == 0 = 1
    | n > 0 = dosala(n) + sum2 (n-1)

--3.1 Listas

descartarPrimeros::(a->Bool)->[a]->[a]
descartarPrimeros f [] = []
descartarPrimeros f (x:xs)
                | f x == True = descartarPrimeros f xs
                | otherwise = x:xs

iguales::Eq a =>a->a->Bool
iguales a b = a == b

exor::Ord a => [a]->[a]->[a]
exor [] [] = []
exor (x:xs) [] = x : exor (descartarPrimeros (iguales x) (x:xs)) []
exor [] (y:ys) = y : exor [] (descartarPrimeros (iguales y) (ys))
exor (x:xs) (y:ys)
    | x == y = exor (descartarPrimeros (iguales x) (x:xs)) (descartarPrimeros (iguales y) (y:ys))
    | x > y = y : exor (x:xs) (descartarPrimeros (iguales y) (ys))
    | x < y = x : exor (descartarPrimeros (iguales x) (xs)) (y:ys)  

unir::[a]->[a]->[a]
unir [] b = b
unir (x:xs) b = x : unir xs b

prefijo::Eq a => [a]->[a]->Bool
prefijo x [] = True
prefijo [] y = False
prefijo (x:xs) (y:ys)
        | y == x = prefijo xs ys
        | otherwise = False