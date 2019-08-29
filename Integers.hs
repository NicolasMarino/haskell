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
