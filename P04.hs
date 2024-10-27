import GHC.Exts.Heap (GenClosure(prof))
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

--recorrido :: Arbol a -> Recorrido -> [a]
--recorrido =

niveles :: Arbol a -> [[a]]
niveles ArbolVacio = []
niveles (Raiz raiz ArbolVacio  ArbolVacio) = [[raiz]]
--niveles (Raiz raiz arbolIzquierdo  arbolDerecho) = [raiz]
                                                                        

minimo :: Arbol a -> a
minimo ArbolVacio = error "Está vacio, no hay minimos"
minimo (Raiz a ArbolVacio _) = a 
minimo (Raiz a arbolIzquierdo _) = minimo arbolIzquierdo

maximo :: Arbol a -> a 
maximo  ArbolVacio = error  "Está vacío, no hya máximos"
maximo (Raiz a _ ArbolVacio) = a
maximo (Raiz a _ arbolDerecho) = maximo arbolDerecho

--eliminar :: Ord a => Arbol a -> a -> Arbol a
--eliminar = 