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
    
