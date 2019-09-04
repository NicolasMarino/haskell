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
  | b == 0 = error "no se puede dividir entre 0"
  | a == b = 1
  | a > b = 1 + divisionEntera (a-b) b
  | otherwise = 0

--resto de la division de dos naturales usando resta.
restoDivision :: Int -> Int -> Int
restoDivision a b
  | b == 0 = error "no se puede dividir entre 0"
  | a == b = 0
  | a > b = restoDivision (a-b) b
  | otherwise = a

productoUsandoSuma::Int->Int->Int
productoUsandoSuma x 1 = x
productoUsandoSuma x y = x + productoUsandoSuma x (y - 1 )


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
            | (y < x) = error "El segundo valor debe ser mayor que el primero."
            | (y > x) = x + sumaNaturales (x+1) y
            | (x <= y) = x

--Ej 11, generalizar la funcion anterior, para ue recubia una funcion f y lo aplque a cada elemento de la sumatoria. 

sumaGeneral::(Int->Int)->Int->Int->Int
sumaGeneral f a b
          | (b < a) = error "El segundo valor debe ser mayor que el primero"
          | a == b = f a
          | b > a = (f b) + (sumaGeneral (f) (a) (b-1))

--Escribir sumG utilizando la funcion anterior (sumaGeneral)

sumgversiondos::(Int->Int)->Int->Int
sumgversiondos f a = (sumaGeneral (f) (0) (a))

-- funcion que recibe dos naturales n y x y retorna si x es divisor de n

divisor::Int->Int->Bool
divisor a b = (mod a b) == 0

-- Funcion que recibe 1 natural,i>2que retorne el primer divisor de i que sea mayor que 1. *(14)

primerDivisor::Int->Int
primerDivisor a = auxiliar a 2 

auxiliar::Int->Int->Int
auxiliar a b
  |divisor a b = b
  |not(divisor a b) = auxiliar a (b+1)

--Usando el anterior, crear una funcion que determine si un numero es primo (15)

esPrimo::Int->Bool
esPrimo a = primerDivisor a == a

-- Que hace?
--        Retorna el primer valor que cumple con el predicado
-- Que retorna si ningun valo del intervalo cumple con el predicado?
--        Retorna b + 1.

--minimo_acotado (16)
minimo_acotado::(Int->Bool)->Int->Int->Int
minimo_acotado p a b
            | a > b = a
            | a <= b && p a = a
            | a <= b && not(p a) = minimo_acotado p (a+1) b

--Reprogramar 14 con el 16

primerDivisorDos::Int->Int
primerDivisorDos n = minimo_acotado (divisor n) 2 n

--Intentando arreglar el primerDivisorDos si le paso 1
--primerDivisorDosDos::Int->Int
--primerDivisorDosDos n = minimo_acotado (\y -> case y of {1 -> (\x -> False);
--                                                       otherwise -> (divisor n);}) 2 n
  
  