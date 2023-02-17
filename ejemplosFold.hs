{- En muchas ocasiones estamos interesados en crear una funci�n que combina todos
los elementos de una lista para obtener un �nico resultado conjunto. Veamos algunos
ejemplos. -}

--predefinida como sum
sumatorio :: Num a => [a] -> a
sumatorio [] = 0
sumatorio (x:xs) = x + sumatorio xs

--predefinida como product
productorio :: Num a => [a] -> a
productorio [] = 1
productorio (x:xs) = x * productorio xs

--predefinida como and
yDeLista :: [Bool] -> Bool
yDeLista [] = True
yDeLista (x:xs) = x && yDeLista xs

--predefinida como length
longitud :: [a] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

--predefinida como concat
aplanar :: [[a]] -> [a]
aplanar [] = []
aplanar (xs:xss) = xs ++ aplanar xss

{- En todos los ejemplos anteriores siempre hacemos lo mismo. En el fondo, lo que 
estamos haciendo es "sustituir" el constructor : por un operador (o funci�n) y 
sustituir la lista [] por una cierta constante. Por ejemplo, al calcular 
   sumatorio (3 : 1 : 2 : 4 : [])     
tenemos       3 + 1 + 2 + 4 + 0
De esta forma, computamos un valor a partir de todos los elementos de la lista.
Podemos generalizar el esquema que aparece en las funciones anteriores sin m�s que
pasar como par�metros adicionales el operador y la constante a usar: -}

--predefinido como foldr
mifoldr :: (a->b->b) -> b -> [a] -> b
mifoldr f e [] = e
mifoldr f e (x:xs) = f x (mifoldr f e xs)

{- Una vez que tenemos este esquema, todas las anteriores definiciones de funciones
podr�an haberse hecho mucho m�s sencillas, sin tener que hacer definiciones recursivas,
solo llamando al esquema general. Hag�moslo, usando ya la funci�n predefinida foldr. -}

sumatorio' xs = foldr (+) 0 xs
productorio' xs = foldr (*) 1 xs
yDeLista' xs = foldr (&&) True xs
longitud' xs = foldr f 0 xs
  where f _ x = x + 1
aplanar' xss = foldr (++) [] xss

{- Es m�s, podemos incluso escribirlo m�s corto. En particular, podemos ahorrarnos
escribir el par�metro xs, abusando de notaci�n relativa a aplicaci�n parcial de funciones
y escribir: -}

sumatorio'' = foldr (+) 0
productorio'' = foldr (*) 1
yDeLista'' = foldr (&&) True
longitud'' = foldr f 0
  where f _ x = x + 1
aplanar'' = foldr (++) []

{- En todos los casos menos longitud se cumpl�a que ten�amos un operador que hac�a 
exactamente lo que nosotros necesit�bamos. En el caso de longitud, dado que no exist�a,
lo podemos definir en un where auxiliar como hemos hecho. Tambi�n, si lo preferimos,
podemos definirlo como una funci�n an�nima, que se definen sin necesidad de proporcionarles
un nombre. La forma de hacerlo ser�a la siguiente: -}

miLong = foldr (\ _ x -> x + 1) 0

{- Otro ejemplo que se ajusta bien al esquema foldr ser�a el algoritmo treeSort, donde
tenemos que meter todos los elementos de una lista en un �rbol binario de b�squeda, para 
luego hacer el recorrido en inorden y obtener todos los elementos ordenados. La idea ser�a
as�: 
treeSort xs = inOrden (foldr anadir AVacio xs)


Otro ejemplo m�s:

factorial n = foldr (*) 1 [1..n]

-}

{- En todos los ejemplos anteriores estamos usando foldr porque estamos asociando por la 
derecha. Por ejemplo, en el sumatorio lo que estamos haciendo realmente es
              3 + (1 + (2 + (4 + 0)))
Realmente podr�amos haber otenido lo mismo asociando por la izquierda haciendo
              (((0 + 3) + 1) + 2) + 4
En este caso el resultado final ser�a el mismo, pero en otros casos no, en particular cuando
el operador no sea conmutativo o no sea asociativo. 
Si queremos asociar por la izquierda, tendremos que usar otro esquema parecido llamado
foldl que hace b�sicamente lo mismo pero asociando por la izquierda. Veamos primero un
ejemplo donde ser�a �til: pasar de una lista de d�gitos a su representaci�n decimal correspondiente.
Es decir, si nos pasan        3 : 1 : 2 : 4: [] queremos devolver el n�mero 3124.
Esto puede computarse as�:  (((0*10+3)*10+1)*10+2)*10+4
Puede observarse la similitud con el esquema fold, donde nuestro operador multiplicar� por 10
el acumulador que tenemos hasta ahora y luego sumar� un nuevo d�gito. La implementaci�n m�s
directa usar�a un par�metro adicional para representar dicho acumulador: -}

numero xs = numeroAux 0 xs
numeroAux acum [] = acum
numeroAux acum (x:xs) = numeroAux (10*acum + x) xs

{- Que podemos generalizar al esquema foldl: -}

mifoldl :: (b->a->b) -> b -> [a] -> b
mifoldl f e [] = e
mifoldl f e (x:xs) = foldl f (f e x) xs

{- De modo que nuestra funci�n numero podr�a haberse escrito directamente como: -}

numero' = foldl (\ acum x -> acum*10 + x) 0

{- La familia fold se completa con otras dos funciones (foldr1 y foldl1) que son �tiles cuando
el esquema es b�sicamente el mismo pero no tiene sentido trabajar con listas vac�as, sino que
el caso base est� en las listas unitarias. Por ejemplo, si quiere calcular el m�ximo de una lista
no tiene sentido calcularlo para listas vac�as, el caso base es la lista unitaria, cuyo m�ximo
elemento es el primer (y �nico) elemento de la lista: -}

maximo :: Ord a => [a] -> a
maximo [x] = x
maximo (x:xs) = max x (maximo xs)

{- La funci�n foldr1 se definir�a de la siguiente forma: -}

mifoldr1 :: (a->a->a) -> [a] -> a
mifoldr1 f [x] = x
mifoldr1 f (x:xs) = f x (mifoldr1 f xs)

{- Y nuestra funci�n m�ximo podr�a haberse escrito m�s f�cilmente como: -}

maximo' :: Ord a => [a] -> a
maximo' = foldr1 max

{- Mientras que la foldl1 ser�a: -}

mifoldl1 :: (a->a->a) -> [a] -> a
mifoldl1 f (x:xs) = foldl f x xs

{- La familia scan puede verse como una generalizaci�n de la familia fold, donde no solo devolvemos
el resultado final, devolvemos la lista de todos los resultados intermedios que ir�a computando el
fold correspondiente. Por ejemplo, si ejecutamos 
  scanl (*) 1 [1..5]
nos devolver�a la lista
  [1,1,2,6,24,120]
Podemos definir las funciones scan de la siguiente forma: -}

miscanl :: (a->b->a) -> a -> [b] -> [a]
miscanl f e [] = [e]
miscanl f e (x:xs) = e : scanl f (f e x) xs

miscanl1 :: (a->a->a) -> [a] -> [a]
miscanl1 f [] = []
miscanl1 f (x:xs) = miscanl f x xs

miscanr :: (a->b->b) -> b -> [a] -> [b]
miscanr f e [] = [e]
miscanr f e (x:xs) = f x e' : (e':es)
  where (e':es) = scanr f e xs
  
miscanr1 :: (a->a->a) -> [a] -> [a]  
miscanr1 f [] = []
miscanr1 f [x] = [x]
miscanr1 f (x:xs) = f x e : (e:es)
  where (e:es) = scanr1 f xs
  
{- N�tese que siempre se cumple que
    last (scanl f z xs) == foldl f z xs
    head (scanr f z xs) == foldr f z xs
-}

{- Por ejemplo, podemos sacar la lista de todos los factoriales hasta un cierto n mediante: -}

factorialesHastaN n = scanl (*) 1 [1..n]    