
data Arbol a = ArbolVacio | Raiz a (Arbol a) (Arbol a) deriving Show

longitud :: Arbol a -> Int
longitud ArbolVacio = 0 
longitud (Raiz a arbi arbd) = 1 + longitud arbi + longitud arbd  

profundidad :: Arbol a-> Int
profundidad ArbolVacio = 0
profundidad (Raiz a arbi arbd) = 1 + max(profundidad arbi) (profundidad arbd)

ancho :: Arbol a-> Int
ancho ArbolVacio = 0
ancho (Raiz a ArbolVacio ArbolVacio) = 1
ancho (Raiz a arbi arbd) = ancho arbi + ancho arbd

data Recorrido = InOrder | PreOrder | PostOrder
recorrido :: Arbol a -> Recorrido -> [a]
recorrido ArbolVacio _ = []
recorrido (Raiz a arbi arbd) InOrder =  recorrido arbi InOrder ++ [a] ++ recorrido arbd InOrder
recorrido (Raiz a arbi arbd) PreOrder = [a] ++ recorrido arbi PreOrder ++ recorrido arbd PreOrder
recorrido (Raiz a arbi arbd) PostOrder =  recorrido arbi PostOrder ++ recorrido arbd PostOrder ++ [a]


niveles :: Arbol a -> [[a]]
niveles ArbolVacio = []
niveles (Raiz a ArbolVacio  ArbolVacio) = [[a]]
niveles (Raiz a arbolIzquierdo  arbolDerecho) = [a] 
                                                   : combinarNiveles(niveles arbolIzquierdo)
                                                                   (niveles arbolDerecho)
                                                                          
combinarNiveles :: [[a]] -> [[a]] -> [[a]]
combinarNiveles [] ys = ys
combinarNiveles xs [] = xs
combinarNiveles (x:xs) (y:ys) = (x++y) : combinarNiveles xs ys

minimo :: (Ord a) => Arbol a -> a
minimo ArbolVacio = error "Está vacío, no hay mínimos"
minimo (Raiz a ArbolVacio ArbolVacio) = a
minimo (Raiz a ArbolVacio arbolDerecho) = min a (minimo arbolDerecho)
minimo (Raiz a arbolIzquierdo ArbolVacio) = min a (minimo arbolIzquierdo)
minimo (Raiz a arbolIzquierdo arbolDerecho) = min a (min (minimo arbolIzquierdo) (minimo arbolDerecho))

maximo :: (Ord a) => Arbol a -> a
maximo ArbolVacio = error "Está vacío, no hay maximos"
maximo (Raiz a ArbolVacio ArbolVacio) = a
maximo (Raiz a ArbolVacio arbolDerecho) = max a (minimo arbolDerecho)
maximo (Raiz a arbolIzquierdo ArbolVacio) = max a (minimo arbolIzquierdo)
maximo (Raiz a arbolIzquierdo arbolDerecho) = max a (max (minimo arbolIzquierdo) (minimo arbolDerecho))

--Intente reducir los casos base pero no me dejaba evaluar un arbol con uno vacío, me decía que tenia que agregar un (Eq) pero mejor lo deje con casos bases, espero no haya problema

eliminar :: Ord a => Arbol a -> a -> Arbol a
eliminar ArbolVacio elemento = error "Está vacío"
eliminar (Raiz x ArbolVacio arbolDerecho) elemento = if x == elemento
                                                    then arbolDerecho
                                                    else error "No está en el arbol"
eliminar (Raiz x arbolIzquierdo ArbolVacio) elemento = if x == elemento
                                                       then arbolIzquierdo
                                                       else error "No está en el árbol"
eliminar (Raiz x arbolIzquierdo arbolDerecho) elemento = if elemento < x
                                                         then(Raiz x(eliminar arbolIzquierdo elemento) arbolDerecho)
                                                          else if elemento > x
                                                          then (Raiz x (eliminar arbolDerecho elemento) arbolIzquierdo)       
                                                          else error "Caso no evalulado"

                                              
