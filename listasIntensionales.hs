{- Las listas intensionales son una notación especial que nos permite escribir programas más
cortos cuando trabajamos con listas. Por ejemplo
  [f x | x <- xs]
debe leerse como "para cada x de la lista xs, aplicamos f a x", es decir, es equivalente a 
  map f xs  
Por su parte, si escribimos
  [x | x <- xs, p x]
debe leerse como "para cada x de la lista xs que cumple p x, devolvemos x", es decir, equivale a
  filter p xs
Finalmente, si escribimos 
  [(x,y) | x <- xs, y <- ys]
debe leerse "para cada x de la lista xs y para cada y de la lista ys, devolvemos la tupla (x,y).
Nótese que esto NO es un zip, pues queremos combinar cada x con cada y, no simplemente primero
con primero, segundo con segundo, etc. Es decir, esto nos devuelve el "producto escalar"  entre
ambas listas. Por ejemplo
  [(x,y) | x <- [1,5,2], y <- [3,8]]
devolvería
  [(1,3),(1,8),(5,3),(5,8),(2,3),(2,8)]
También podemos complicar los casos de uso haciendo que unos "generadores" dependan de otros.
Por ejemplo:
  [(x,y) | x <- [1,5,2], y <- [1..x]]
devolvería
  [(1,1),(5,1),(5,2),(5,3),(5,4),(5,5),(2,1),(2,2)]
O incluso podemos usar casos un tanto extraños como:
  [7 | x <- [1..5]]
que devolvería 
  [7,7,7,7,7]
O incluso casos más raros como
  [x | x <- xs, False]
que devolvería
  []
En general, podemos mezclar tantos generadores y condiciones como queramos:
  [(x,y) | x <- [1..4], even x, y <- [x+1..4], odd y]  
Veamos ahora algunos ejemplos un poco más útiles.  
-}

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort [y|y<-xs,y<=x] ++ [x] ++ qsort [y|y<-xs,y>x]

divisores :: Integral a => a -> [a]
divisores n = [d | d<-[1..n], mod n d == 0]

primo :: Integral a => a -> Bool
primo n = divisores n == [1,n]

primo' :: Integral a => a -> Bool
primo' n = null [d | d <- [2..raiz], mod n d ==0] && (n>1)
  where raiz = floor (sqrt (fromIntegral n))
  
--Supongamos ahora que queremos resolver el problema de colocar 8 reinas en un
--tablero de ajedrez de modo que ninguna de ellas ataque a ninguna otra. Vamos
--a calcular la lista de todas las posibles soluciones, donde cada solución es
--una lista de coordenadas donde colocar las damas
ochoreinas :: [[(Int,Int)]]  
ochoreinas = reinas 8

--Lo hacemos de forma recursiva. reinas n nos dirá todas las formas de poner
--n reinas en las primeras n columnas de un tablero 8x8
reinas 0 = [[]]  -- Existe una única forma de colocar 0 reinas, que consiste en no colocar nada (lista vacía)
reinas n = [(n,nuevo):previo | previo <- reinas (n-1), nuevo <- [1..8], noAtaques previo (n,nuevo)]
--Es decir, combinamos todas las opciones que teníamos para poner (m-1) reinas con todas las opciones que
--tenemos para poner la m-ésima reina, que lógicamente son 8 (las 8 posibles filas de la m-ésima columna).
--Eso sí, después tenemos que poner la condición de que la nueva reina no ataque a las previas, donde para
--eso tenemos que comprobar si hay ataque o no a cada una:

noAtaques :: [(Int,Int)] -> (Int,Int) -> Bool
noAtaques lista nueva = and [comprueba previa nueva | previa <- lista]

--Tenemos que comprobar que no están en la misma fila ni en las mismas diagonales. No hace falta comprobar
--la columna, porque por construcción siempre hemos puesto que la nueva reina va a una columna nueva.
comprueba :: (Int,Int) -> (Int,Int) -> Bool
comprueba (i,j) (m,n) = (j/=n) && (i+j/=m+n) && (i-j/=m-n)