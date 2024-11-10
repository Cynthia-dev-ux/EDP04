
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

minimo :: Arbol a -> a
minimo ArbolVacio = error "Está vacio, no hay minimos"
minimo (Raiz a _ ArbolVacio) = a 
minimo (Raiz _ _ arbolIzquierdo) = minimo arbolIzquierdo


maximo :: Arbol a -> a 
maximo  ArbolVacio = error  "Está vacío, no hay máximos"
maximo (Raiz a ArbolVacio _) = a
maximo (Raiz _  arbolDerecho _) = maximo arbolDerecho


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

                                              
