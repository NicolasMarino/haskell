module Integers where


sumatoria::Int->Int
sumatoria 0 = 0
sumatoria n = n + sumatoria ( n - 1 )

factorial::Int->Int
factorial 0 = 1
factorial a = a * factorial(a - 1)

potencia::Int->Int
potencia 0 = 1
potencia b = 2 * potencia(b - 1)

potenciaAlaN::Int->Int->Int
potenciaAlaN a 0 = 1
potenciaAlaN a b = a * potenciaAlaN a (b - 1)

repetirString::String->Int->String
repetirString s 1 = s
repetirString s n = s ++ repetirString s (n-1)

divisionEntera :: Int -> Int -> Int
divisionEntera a b
  | a == b = 1
  | a > b = 1 + divisionEntera (a-b) b
  | otherwise = 0

restoDivision :: Int -> Int -> Int
restoDivision a b
  | a == b = 0
  | a > b = restoDivision (a-b) b
  | otherwise = a

productoUsandoSuma::Int->Int->Int
productoUsandoSuma x 1 = x
productoUsandoSuma x y = x + productoUsandoSuma x (y - 1 )

--division dos naturales usando resta
divNresta::Int->Int->Int
divNresta x y
        | x == 0 = error "no se puede dividir entre 0"
        | y < x  = 1 + (divNresta (x-y) y)
        | x == y = 1
        | otherwise = 0

--resto dos naturales usando resta.
restonresta::Int->Int->Int
restonresta x y
        | x == 0 = error "no se puede dividir entre 0"
        | x < y = x
        | y <= x  = divNresta (x-y) y
--suma g
sumg::(Int->Int)->Int->Int
sumg f n
    | n == 0 = f 0
    | n > 0 = f n + sumg f (n-1)

alcubo::Int->Int
alcubo n = n ^ 3

elevaralcubo::Int->Int
elevaralcubo n = sumg(alcubo) n

-- 6.2 Función que computa la suma de los primeros n n´umeros impares.

imparesHastaN :: Int -> Int
imparesHastaN n
    | n <= 0 = 0
    | (mod n 2) == 0 = imparesHastaN (n-1)
    | otherwise = n + imparesHastaN (n-2)
                 
-- 8.1. Programar una función que compute la suma de los naturales i tales que a ≤ i ≤ b para a y b dados.

sumaNaturales::Int->Int->Int
sumaNaturales x y
            | (y > x) = x + sumaNaturales (x+1) y
            | (x <= y) = x
            | (y < x) = error "El primer valor debe ser mayor que el segundo."

        