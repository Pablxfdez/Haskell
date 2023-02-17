module ABB (ABB,crearABBVacio,estaVacio,anadir,eliminar,inOrden) where
-- Si quisiéramos exportar también los constructores del tipo, exportaríamos ABB(..) en lugar de ABB.
-- Tal y como está, exportamos ABB como tipo abstracto.


data ABB a = Vacio | Nodo a (ABB a) (ABB a)
  deriving (Eq,Show,Read)

estaVacio :: ABB a -> Bool
estaVacio Vacio = True
estaVacio _ = False

crearABBVacio :: ABB a
crearABBVacio = Vacio

anadir :: Ord a => a -> ABB a -> ABB a
anadir x Vacio = Nodo x Vacio Vacio
anadir x (Nodo y iz dr)
  | x > y = Nodo y iz (anadir x dr)
  | otherwise = Nodo y (anadir x iz) dr
  
eliminar :: Ord a => a -> ABB a -> ABB a
eliminar x Vacio = Vacio
eliminar x (Nodo y iz dr)
  | x > y = Nodo y iz (eliminar x dr)
  | x < y = Nodo y (eliminar x iz) dr
  | estaVacio iz = dr
  | otherwise = Nodo maxI (eliminar maxI iz) dr
  where maxI = maximo iz

maximo :: ABB a -> a
maximo (Nodo x iz dr)
  | estaVacio dr = x
  | otherwise = maximo dr  
  
inOrden :: ABB a -> [a]
inOrden Vacio = []
inOrden (Nodo x iz dr) = inOrden iz ++ [x] ++ inOrden dr
