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
