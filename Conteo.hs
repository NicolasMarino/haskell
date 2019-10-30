module Conteo where

agregar01:: [Int] -> [[Int]]
agregar01 xs = [(xs++[0]),(xs++[1])]

agregarBit :: [[Int]] -> [[Int]]
agregarBit [] =  []
agregarBit (x:xs) = agregar01 x ++ agregarBit xs 

posiblesNBits:: Int -> [[Int]]
posiblesNBits 0 = []
posiblesNBits 1 = agregar01 []
posiblesNBits n = agregarBit (posiblesNBits (n-1))

------------------------------------------------Permutaciones con repeticion--------------------------------------------------------------------------------------
numEntre:: Int -> Int -> [Int]
numEntre a b
    |a == b = [a]
    |a < b = a: numEntre (a+1) (b)

agregarTodos :: [a] -> [a] -> [[a]]
agregarTodos [] _ = []
agregarTodos (x:xs) (ys) = (x:ys): agregarTodos xs ys 

agregar06:: [Int] -> [[Int]]
agregar06 = agregarTodos (numEntre 0 6)

agregarDigito:: [[Int]] -> [[Int]]
agregarDigito [] = [] 
agregarDigito (x:xs) =  agregarDigito xs ++ agregar06 x

posiblesNDigitos :: Int -> [[Int]]
posiblesNDigitos 0 = []
posiblesNDigitos 1 = agregar06 []
posiblesNDigitos n = agregarDigito (posiblesNDigitos (n-1))

----------------------------------------------Permutaciones-----------------------------------------------------------------------------------------
agregarTodosFiltrando ::Eq a=> [a] -> [a] -> [[a]]
agregarTodosFiltrando [] _ = []
agregarTodosFiltrando (x:xs) (ys)
    | elem x ys = agregarTodosFiltrando xs ys 
    | not (elem x ys) = (x:ys): agregarTodosFiltrando xs ys 

agregarEntre:: Int -> Int -> [Int] -> [[Int]]
agregarEntre a b = agregarTodosFiltrando (numEntre a b)

agregarNumeroEntre:: Int -> Int -> [[Int]] -> [[Int]]
agregarNumeroEntre a b [] = [] 
agregarNumeroEntre a b (x:xs) =  agregarNumeroEntre a b xs ++ agregarEntre a b x

posibles5DeOro :: Int -> Int -> Int -> [[Int]]
posibles5DeOro a b 0 = []
posibles5DeOro a b 1 = agregarEntre a b []
posibles5DeOro a b n = agregarNumeroEntre a b (posibles5DeOro a b (n-1))
---------------------------------------------Combinaciones------------------------------------------------------------------------------------------
contiene :: Eq a => [a] -> [a] -> Bool
contiene [] [] = True
contiene _ [] = False
contiene [] _ = False
contiene (x:xs) (ys) = elem x ys && contiene xs (filter (\y -> y /= x) ys)

quitarRepetidos :: Eq a =>[[a]] -> [[a]]
quitarRepetidos [] =[]
quitarRepetidos (x:xs) = x: quitarRepetidos (filter (\y -> not(contiene x  y)) xs)

posibles5DeOroReal::Int -> Int -> Int->  [[Int]]
posibles5DeOroReal a b n = quitarRepetidos (posibles5DeOro a b n)

---------------------------------------------Regla de la suma y la multiplicacion--------------------------------------------------------------------------------------
aCaracteres :: [Int] ->[Char]
aCaracteres xs = map (\x -> toEnum x) xs

letras :: [Char]
letras = aCaracteres (numEntre (fromEnum 'a') (fromEnum 'z'))

digitoAChar :: Int -> Char
digitoAChar d = head (show d)

agregarNumero :: Int -> [Char] -> [[Char]]
agregarNumero a [] =[]
agregarNumero a (x:xs) = [digitoAChar(a),x] : agregarNumero a xs

concatenarNumerosYLetras :: [Int] -> [Char] ->[[Char]]
concatenarNumerosYLetras [] ys = []
concatenarNumerosYLetras (x:xs) ys = agregarNumero x ys ++ concatenarNumerosYLetras xs ys

aListaDeListas:: [Char] -> [[Char]]
aListaDeListas xs = map (\c -> [c]) xs

--------------------------------------- NUEVO -------------------------------------------------------------------------------

-----------------------------------------ReglaSumaGeneral--------------------------------------------------------------------
reglaSuma :: Eq a => [[a]] -> [[a]] -> [[a]]
reglaSuma xs ys = xs ++ ys

------------------------------------------ReglaMultiplicacionGeneral-------------------------------------------------------------------------------

concatListas :: [a] -> [[a]] -> [[a]]
concatListas _ [] = []
concatListas xs (y:ys) = (xs++y) : concatListas xs ys

reglaMultiplicacion:: [[a]] -> [[a]] -> [[a]]
reglaMultiplicacion [] _= []
reglaMultiplicacion _ [] = []
reglaMultiplicacion (x:xs) (ys) = concatListas x ys ++ reglaMultiplicacion xs ys

-------------------------------------------------------------------------------------------------------------------------


concatTodosParaCU:: [a] -> [[a]] -> [[a]]
concatTodosParaCU _ [] = []
concatTodosParaCU (xs) (y:yss) = agregarTodos xs y ++ concatTodosParaCU xs yss

permutacionesConRep :: Int -> [a] -> [[a]]
permutacionesConRep 0 xs= []	
permutacionesConRep 1 xs = agregarTodos xs []
permutacionesConRep n xs= concatTodosParaCU xs (permutacionesConRep (n-1) xs)

--------------------------------------------------------------------------------------------------------------------------

sacarYaContenidos :: Eq a => [a] -> [a] -> [a]
sacarYaContenidos xs [] = xs
sacarYaContenidos xs (y:ys) = sacarYaContenidos (takeWhile (y/=) xs ++ tail (dropWhile (y/=) xs)) ys

agTodosSinRep:: Eq a=> [a] -> [[a]] -> [[a]]
agTodosSinRep _ [] = []
agTodosSinRep (xs) (y:ys) = agregarTodos (sacarYaContenidos xs y) y ++ agTodosSinRep xs ys

permutaciones:: Eq a => Int -> [a] -> [[a]]
permutaciones 0 xs = []
permutaciones 1 xs = agregarTodos xs []
permutaciones n xs = agTodosSinRep xs (permutaciones (n-1) xs)

---------------------------------------------------------------------------------------------------------------------

combinatoria:: Eq a => Int -> [a] -> [[a]]
combinatoria n xs = quitarRepetidos (permutaciones n xs)

transformarListaDeDigitos :: [Int] -> [Char]
transformarListaDeDigitos xs =  map (digitoAChar) xs

juntos :: [Int] -> [Int] -> Bool -- funcion que verifica si todos los elementos de la primer lista se encuentran contiguos en la segunda.
juntos xs [] = False
juntos (xs) (y:ys) = (length (takeWhile (\n -> elem n xs) (y:ys)) == length xs) || juntos xs ys




