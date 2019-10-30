
module Listas where


largoLista::[Int]->Int
largoLista [] = 0
largoLista (x:xs) = 1 + largoLista xs

--Multiplicacion de los elementos de una lista

porElemLista::[Int]->Int
porElemLista [] = 1
porElemLista (x:xs) = x * porElemLista xs

--Chequear si todos los elementos de una lista son true
checkBoolean::[Bool]->Bool
checkBoolean [] = True
checkBoolean (x:xs)
            | x == False = False
            | x == True = checkBoolean xs

-- chequear si hay al menos un true en una lista
alMenosUnTrue::[Bool]->Bool
alMenosUnTrue [] = False
alMenosUnTrue (x:xs) 
              | x == True = True
              | x == False = alMenosUnTrue xs

alMenosUnTrue2::[Bool]->Bool
alMenosUnTrue2 [] = False
alMenosUnTrue2 (x:xs) = x || alMenosUnTrue2 xs

--Funcion generica para reescribir las anteriores.
reduce::(a->a->a)->a->[a]->a
reduce f b [] = b
reduce f b (x:xs) = f x (reduce f b xs)        

-- Multiplicacion de los elementos de una lista usando reduce
porElemListav2::[Int]->Int
porElemListav2 (x) = reduce (*) (1) (x)

--Chequear si todos los elementos de una lista son true usando reduce
checkBooleanv2::[Bool]->Bool
checkBooleanv2 x = reduce (&&) True x

-- Chequear si hay al menos un true en una lista usando reduce
alMenosUnTruev2::[Bool]->Bool
alMenosUnTruev2 x = reduce (||) False x


--Reduce mas generico
reducev2::(a->b->b)->b->[a]->b
reducev2 f b [] = b
reducev2 f b (x:xs) = f x (reducev2 f b xs)

--Contar elementos
largoListav2::[a]->Int
largoListav2 x = reducev2 (\a n -> 1+n) 0 x

--Programar append que reciba dos listas de un tipo cualquiera y retorne las listas concatenadas
appendListas::[a]->[a]->[a]
appendListas [] b = b
appendListas (x:xs) b = x : appendListas (xs) (b)

--Programar append usando reduce[]
appendReduce::[a]->[a]->[a]
appendReduce xs ys = reducev2 (:) ys xs

--Agregar un elemento al final de la lista
agregarfinal::[a]->a->[a]
agregarfinal [] b = b : []
agregarfinal (x:xs) b = x  : (agregarfinal xs b)

--Recibe una lista de enteros xs y un entero a y determine si a es miembro o no de xs
perteneceInt::[Int]->Int->Bool
perteneceInt [] x = False
perteneceInt (x:xs) a 
    | a == x = True
    | otherwise = (perteneceInt xs a)   

pertenece::Eq a => [a]->a->Bool
pertenece [] a = False
pertenece (x:xs) a 
    | a == x = True
    | otherwise = (pertenece xs a) 

-- dada una lista y un elemento retornar la cantidad de veces que aparece el elemento en la lista 
contarPertenece::Eq a => [a]->a->Int
contarPertenece [] a = 0
contarPertenece (x:xs) a 
    | a == x = 1 + (contarPertenece xs a)
    | otherwise = (contarPertenece xs a)

-- 11 dado un natural y una lista, retornar el elemento en la posicion representada por el natural.
elementoDePosDada::Eq a => [a]->Int->a
elementoDePosDada [] d = error"asd"
elementoDePosDada (x:xs) d 
            | d == 0 = x
            | otherwise = (elementoDePosDada xs (d-1))

-- 12 dado un elemento y una lista, retornar la posicion del elementoen la lista. Si el elemento no esta retornar el largo de la lista
elementoDePosDada2::Eq a => [a]->a->Int
elementoDePosDada2 [] a = 0
elementoDePosDada2 (x:xs) a 
               | a == x = 0
               | otherwise = (1+(elementoDePosDada2 xs a)) 
-- 15

cumpleCond::[a]->(a->Bool)->[a]
cumpleCond [] f = []
cumpleCond (x:xs) f
            | f x = cumpleCond xs f
            | otherwise = cumpleCond xs f

-- dada una lista y un predicado, retornar los primeros n elementos contiguos de la lista, tal que todos los elementos cumplem con el predicado.
-- ejemplo: tomarMientras (esPar) [2,4,6,8,9,2] = [2,4,6]
esPar::Integral a => a->Bool
esPar a = (mod a 2) == 0

predicadoContiguo::Eq a =>[a]->(a->Bool)->[a]
predicadoContiguo [] p = []
predicadoContiguo (x:xs) p
                | ((p x) == True) = (x : (predicadoContiguo xs p))
                | otherwise = []

-- Programar una funcion que dada una lista y un elemento elimine todas las ocurrencias del elemento en la lista.

borrarElemLista::Eq a =>[a]->a->[a]
borrarElemLista [] x = []
borrarElemLista (x:xs) b 
                | x == b = (borrarElemLista xs b)
                | otherwise = x : (borrarElemLista xs b)

-- programar funcion que dada una lista y un elemento elimine la primer ocurrencia del elemento.

borrarPrimerElem::Eq a =>[a]->a->[a]
borrarPrimerElem [] x = []
borrarPrimerElem (x:xs) b 
                | x == b = xs
                | otherwise = x : (borrarPrimerElem xs b)

            

-- Programar tonar n qe dada una lista y un numero retorna los primeros n elementos de la lista

nElem::Eq a =>[a]->Int->[a]
nElem [] b = []
nElem (x:xs) b
        | (b /= 0) = x : (nElem xs (b-1))
        | otherwise = nElem xs b

-- 18.a Programar tomar n que dada una lista y un numero retorna la lista  resultado de descartar los primeros n elementos
borrarNelem::Eq a => [a]->Int->[a]
borrarNelem [] b = []
borrarNelem (x:xs) b
            | (b == 0) = x:xs
            | otherwise = (borrarNelem xs (b-1))


-- 18.b Programar fromTo, que dados a <= b, retorne la lista de todos los numeros entre ay b inclusive. EJ: fromTo 1 3 = [1,2,3]
fromTo::Int->Int->[Int]
fromTo x y 
        | x <= y = x : (fromTo (x+1) y)
        | otherwise = []
-- dado un predicado y una lista me devuelve todos los elementos que cumplan con la condicion
filtrar::[Int]->(Int->Bool)->[Int]
filtrar [] f = []
filtrar (x:xs) f    
        | (f x) = x:filtrar xs f
        | otherwise = filtrar xs f
-- Programar divisores que dado un numero retorna la lista de divisores de dicho numero usar filter y fromto para esto.
divisores::Int->[Int]
divisores n = filtrar (fromTo 1 n) (\x -> mod n x == 0)

--primera posicion retorna la pos que aparece x o length si no
primera_posicion::[Int]->Int->Int
primera_posicion [] n = 0
primera_posicion (x:xs) n
        |(x == n) = 0
        |(x /= n) = 1 + primera_posicion xs n
--Recibe [0] y devuelve [[0,0],[1,0]]
agregar01::[Int]->[[Int]]
agregar01 xs = [(0:xs),(1:xs)]

--agregar bit
agregarBit::[[Int]]->[[Int]]
agregarBit [] = []
agregarBit (x:xs) = (agregar01 x) ++ (agregarBit xs)

--posiblesNBits 2 = [[0,0],[0,1],[1,0],[1,1]]
posiblesNBits::Int->[[Int]]
posiblesNBits 0 = []
posiblesNBits 1 = agregar01 []
posiblesNBits n = agregarBit(posiblesNBits (n-1))

numEntre::Int->Int->[Int]
numEntre x y 
        | x <= y = x : (numEntre (x+1) (y))
        | otherwise = []

numEntrev2::Int->Int->[Int]
numEntrev2 n x
            | (n==x) = [n]
            | otherwise = [n] ++ (numEntrev2 (n+1) x)


agregarTodos::[Int]->[Int]->[[Int]]
agregarTodos [] ys = []
agregarTodos (x:xs) ys = [x : ys] ++ (agregarTodos xs ys)

agregar06::[Int]->[[Int]]
agregar06 n = (agregarTodos (numEntre 0 6) n)

agregarDigito::[[Int]]->[[Int]]
agregarDigito [] = []
agregarDigito (x:xs) = (agregar06 x) ++ (agregarDigito xs)

borrar::Eq a => a -> [a] -> [a]
borrar n [] = []
borrar n (x:xs)
        |n==x = borrar n xs
        |otherwise = x: borrar n xs

agregarTodosFiltrando::[Int]->[Int]->[[Int]]
agregarTodosFiltrando [] (b:bs) = []
agregarTodosFiltrando (x:xs) (b:bs)
               | not(elem x (b:bs)) = [x:b:bs] ++ (agregarTodosFiltrando xs (b:bs))  
               | elem x (b:bs) = agregarTodosFiltrando xs (b:bs) 

-- agregarEntre::Int->Int->[Int]
-- agregarEntre 


agregarTodosFiltrandov2::[Int]->[Int]->[[Int]]
agregarTodosFiltrandov2 [] ys = []
agregarTodosFiltrandov2 (x:xs) (y:ys)
        | ((auxc2 x (y:ys))== True )= agregarTodosFiltrandov2 xs (y:ys)
        | ((auxc2 x (y:ys)) == False) = [x:y:ys] ++ (agregarTodosFiltrandov2 xs (y:ys))

auxc2::Int->[Int]->Bool
auxc2 x [] = False
auxc2 x (y:ys)
        | (x==y) = True 
        | otherwise = auxc2 x ys

agregarEntre2::Int->Int->[Int]->[[Int]]
agregarEntre2 x y l = agregarTodos(numEntre x y) (l)

zipWithS::(Int->Int->Int)->[Int]->[Int]->[Int]
zipWithS f x [] = []
zipWithS f (x:xs) (y:ys) = f x y : zipWithS f xs ys

sumarNico::Int->Int->Int
sumarNico x y = x+y

--2. Funciona que reciba una lista de booleanos y realice la conjuncion logica de toods sus miembros
funBool::[Bool]->Bool
funBool [] = True
funBool (x:xs) = x && (funBool xs)

--3. Programar == para listas de enteros isn hacer recursi'on en su definición.
--igualNico::[Int]->[Int]->Bool
--igualNico (x:xs) =

--3. Programar find que reciba un entero a y una lista de enteros l y retorne la posicion de la primera aparicion de a en l.
--Las posiciones de una lista se numeran desde 0. Si el elemento no aparece en la lista se retornara el largo de esta.
findNico::Int->[Int]->Int
findNico n [] = 0
findNico n (x:xs)
        | n == x = 0
        | n /= x = 1 + findNico n xs
        | otherwise = findNico n xs

--Problema 3
-- 1. Programar una funcion recursiva snoc que agrega un elemento dado al final de una lista dada.
-- 2. Programar la concatenacion (++) de dos listas dadas.
snocAgrega::a->[a]->[a]
snocAgrega a [] = a:[]
snocAgrega a (x:xs) = x : snocAgrega a xs

concatenaRedefinido::[a]->[a]->[a]
concatenaRedefinido [] ys = ys
concatenaRedefinido (x:xs) ys = x : concatenaRedefinido xs ys


--Definir la funci´on cifras::Int -> Int tal que cifras n devuelve la cantidad de d´ıgitos que
--contiene la representaci´on decimal de n. Asumir que el n´umero n es un n´umero entero positivo
cifras::Int->Int
cifras n
        | (n > 10) = 1 + cifras (div n 10)
        | (n < 10) = 1

--Definir la funci´on factorialCre::Int -> Int -> Int tal que factorialCre a n devuelve
--la multiplicaci´on de n n´umeros consecutivos a partir de a
factorialCre::Int->Int->Int
factorialCre a b
          | b >= 1 = a * factorialCre ((a+1)) (b-1) 
          | b < 1 = 1


minimo::[Int]->Int
minimo [x] = x
minimo (x:x2:xs)
        | x < x2 = minimo (x:xs)
        | otherwise = minimo (x2:xs)


largov23::[a]->Int
largov23 [] = 0
largov23 (n:xs) = 1 + largov23 xs

posv23::Int->[a]->a
posv23 n [] = error"404"
posv23 n (x:xs)
        | n == 0 = x
        | n /= 0 = posv23 (n-1) xs

medio::[a]->a
medio ys = posv23 (div (largov23 ys) 2) ys 

--3.a elevar m a la n
expv23::Int->Int->Int
expv23 m n
        | n == 0 = 1   
        | n /= 0 = m * (expv23 m (n-1))

--3.b Demostrar

--Cantidadde contenidos que dado un elemento y una lista xs retorna la cantidad de veces que aparece n en xs
cantidadC::Eq a => a->[a]->Int
cantidadC e [] = 0
cantidadC e (x:xs)
        | x == e = 1 + cantidadC e xs
        | otherwise = cantidadC e xs

--Programar enviarAlFinal que dado un elemento e y una lista xs devuelve una nueva lista con los 
--mismos elementos y todas las ocurrencias de e al final de la lista xs. EJ: 1 [3,1,2,1] = [3,2,1,1]

enviarAlFinal::Eq a=>a->[a]->[a]
enviarAlFinal e [] = []         
enviarAlFinal e (x:xs)
        | x == e = enviarAlFinal e xs ++ [x]
        | otherwise = x : enviarAlFinal e xs

--Demostrar que 

divisibleEntre3 :: Int -> Bool
divisibleEntre3 n
                | n == 0 = True
                | n < 0 = False
                | n > 0 = divisibleEntre3 (n-3)

multiplicado :: Int -> Int -> Int
multiplicado n m 
        | m > 0 = n + multiplicado n (m-1)
        | m <= 0 = 0

sum1 :: Int->Int
sum1 1 = 1
sum1 n = (3*n) - 2 + sum1 (n-1)

dosala :: Int -> Int
dosala 0 = 1
dosala n = 2 * dosala (n-1)

sum2::Int->Int
sum2 0 = 1
sum2 n = dosala n + sum2 (n-1)

consii::Bool->Bool->Bool->Bool
consii a b c = (not((not((not a) || (not(b) || c)) || ((not b)||c || a))))

-- contiene::Eq a =>[a]->[a]->Bool
-- contiene [] [] = True
-- contiene [] ys = False
-- contiene xs [] = False
-- contiene (x:xs) (y:ys) = elem x ys && contiene xs (filter ())
--         | otherwise = False
        