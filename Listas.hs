--suma de todos los elementos de una lista
suma::[Int]->Int
suma[] = 0
suma(x:xs) = x + suma xs

--numero total de elementos de una lista
totalLista::[a]->Int
totalLista [] = 0
totalLista (x:xs) = 1 + totalLista xs

--producto de todos los elementos de la lista
productoLista::[Int]->Int
productoLista [] = 1
productoLista (x:xs) = x * productoLista xs

--devuelve si todos los elementos son True
todosTrue::[Bool]->Bool
todosTrue [] = True
todosTrue (x:xs)
    | x == True = todosTrue xs
    | x == False = False

--devuelve si hay al menos un elemento True
hayTrue::[Bool]->Bool
hayTrue [] = False
hayTrue (x:xs)
    | x == True = True
    | x == False = hayTrue xs
    
reduce::(a->a->a)->a->[a]->a
reduce f b [] = b
reduce f b (x:xs) = f x (reduce f b xs)

append::[a]->[a]->[a]
append[] ys = ys
append (x:xs) ys = x:append xs ys

invertir1::[a]->[a]
invertir1[] = []
invertir1(x:xs) = append (invertir1 xs) (x:[])

reduce::(a->b->b)->b->[a]->b
reduce f b [] = b
reduce f b (x:xs) = f x (reduce f b xs)

invR::[a]->[a]
invR xs = reduce agF [] xs

agF::a->[a]->[a]
agF x ys = append ys [x]

aplanar::[[a]]->[a]
aplanar [] = []
aplanar (x:xs) = append x (aplanar xs)

aplanarR::[[a]]->[a]
aplanarR xs = reduce (\x ys -> append x ys) [] xs

esVacia::[a]->Bool
esVacia[] = True
esVacia _ = False

cabeza::[a]->a
cabeza[] = error "La lista esta vacia"
cabeza (x:xs) = x

cola::[a]->[a]
cola[] = []
cola (x:xs) = xs

ultimoLista::[a]->a
ultimoLista [] = error "Lista vacia"
ultimoLista (x:[]) = x
ultimoLista (x:xs) = ultimoLista xs

-- retornar todos menos el ultimo
quitarUltimo::[a]->[a]
quitarUltimo [] = error "Lista vacia"
quitarUltimo [x] = []
quitarUltimo (x:xs) = x:(quitarUltimo xs)

-- dada una lista y un elemento, agregar el elemento al final de la lista recursivo
agregarFinal::[a]->a->[a]
agregarFinal [] x = [x]
agregarFinal (x:xs) b = x : (agregarFinal xs b)

-- funcion que recibe una lista de enteros xs y un entero a y t=determine si a es miembro o no de xs
perteneceInt::[Int]->Int->Bool
perteneceInt [] _ = False
perteneceInt (x:xs) a
    | a == x = True
    | otherwise = perteneceInt xs a

-- misma funcion para tipo cualquiera a
pertenece::Eq a=>[a]->a->Bool
pertenece [] _ = False
pertenece (x:xs) a
    | a == x = True
    | otherwise = pertenece xs a

      
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
