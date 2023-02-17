-- Las siguientes funciones hacen b�sicamente lo mismo:

adicion (x,y) = x + y

suma x y = x + y

{- Ahora bien, en Haskell suele haber m�s tendencia a escribir la segunda que la primera. El
motivo es que la segunda est� "currificada", es decir, recibe sus par�metros por separado.
La principal ventaja de esto es que es posible aplicar dichas funciones parcialmente. 
Si una funci�n (u operador) tiene n par�metros y solo le pasamos m (con m < n) entonces nos
devolver� una nueva funci�n que esperar� los n-m par�metros restantes. Dicha nueva funci�n
puede usarse de forma completamente normal, almacenarla en estructuras de datos, pasarla 
como par�metro de otra funci�n, etc. Por ejemplo, podemos hacer

   map (suma 3) [8,2,5,4]

pero no podemos hacer
   
   map (adicion 3) [8,2,5,4]   
   
Ahora bien, jugando con funiones de orden superior puedo convertir cualquier funci�n no-currificada
en currificada, y viceversa. Para convertir de no-currificada a currificada est� predefinida la
funci�n curry, que podr�amos definir as�: -}

micurry :: ((a,b) -> c) -> (a -> b -> c)
micurry f x y = f (x,y)

{- Es decir, cuando recibe la funci�n no-currificada y recibe tambi�n por separado los dos par�metros 
que formar�an la tupla de entrada de dicha funci�n, aplica la funci�n no-currificada sobre la tupla
que acabamos de formar.
Ahora bien, dado que micurry tiene 3 par�metros por separado, es posible aplicarla parcialmente. En
particular, si le pasamos solo su primer par�metro (que es una funci�n) nos devolver� una nueva 
funci�n que espera los otros dos par�metros por separado. Es decir, nos devuelve una funci�n currificada
que espera dos par�metros.

N�tese que el tipo  
    ((a,b) -> c) -> (a -> b -> c) 
puede leerse como "dada una funci�n no-currificada que espera una tupla, devuelve una funci�n 
currificada que espera los dos elementos por separado". Ahora bien, dicho tipo es equivalente a 
    ((a,b) -> c) -> a -> b -> c
es decir, dada una funci�n no currificada, y dadas otras dos entradas de tipo a y b, devuelve 
algo de tipo c.    

As� pues, si queremos aplicar parcialmente la funci�n no-currificada adicion, podr�amos escribir
  
  map (uncurry adicion 3) [8,2,5,4]
-}

{- Cuando aplicamos parcialmente un operador binario, podemos hacerlo indistintamente pasando solo
el primer par�metro o solo el segundo. Por ejemplo

  map (^2) [1,2,3,4]

devuelve los cuadrados de los elementos de la lista, mientras que 

  map (2^) [1,2,3,4]
  
devuelve cuatro potencias de 2.
Ahora bien, cuando aplicamos parcialmente una funci�n, siempre debemos hacerlo en el orden en el
que tenemos los par�metros, es decir, solo podemos aplicar parcialmente los primeros par�metros. 
Por ejemplo, lo siguiente es correcto:

  map (take 3) ["esto","es","una","prueba"]

devuelve  ["est","es","una","pru"] pero si quisi�ramos aplicar parcialmente el take con su segundo
par�metro (por ejemplo, para hacer algo as� como map (take "prueba") [1,3,2,4] con el objetivo
de sacar 1, 3, 2 o 4 caracteres de la palabra prueba, entonces no funcionar�a.
Ahora bien, podemos definir funciones de orden superior que dada una funci�n con dos par�metros
nos devuelva otra funci�n equivalente pero que espere los par�metros en el orden opuesto. En particular,
flip le da la vuelta al orden de los dos primeros par�metros: -}

miflip :: (a -> b -> c) -> (b -> a -> c)
miflip f y x = f x y

{- N�tese que lo �nico que hace flip es invocar a la funci�n f, pero pasando los par�metros en el 
orden que necesita f. Gracias a esta funci�n, podemos escribir cosas como

  map (flip take "prueba") [1,3,2,4]
  
N�tese tambi�n que la funci�n flip tambi�n vale para funciones que tengan m�s de dos par�metros de
entrada. Ahora bien, lo �nico que har�a en dichos casos es intercambiar el orden de los dos primeros.
Si queremos pasar a que el tercer par�metro sea el primero, habr�a que definir otra funci�n flip3
que haga lo que queremos. -}

{- En general, podemos definir tantas funciones y operadores de orden superior como nos interese.
Algunas �tiles que tenemos predefinidas son:

--Aplicaci�n de funci�n que asocie a derechas. �til para escribir menos par�ntesis
infixr 0 $
($) :: (a -> b) -> a -> b
f $ x = f x

--Composici�n de funciones
(.) :: (a -> b) -> (c -> a) -> (c -> b)
(.) f g x = f (g x)

-}
  
--Aplicar dos veces una misma funci�n: predefinida como twice
mitwice :: (a -> a) -> a -> a
mitwice f = f . f

--Aplicar infinitas veces una misma funci�n, devolviendo todos los resultados intermedios.
--Predefinida como iterate
miiterate :: (a -> a) -> a -> [a]
miiterate f x = x : iterate f (f x)

-- Por ejemplo, si queremos sacar la lista de d�gitos de un n�mero natural, podemos escribir:
digitos :: Int -> [Int]
digitos n= reverse (map (`mod` 10) (takeWhile (/=0) (iterate (`div`10) n)))

-- O si queremos poner menos par�ntesis:
digitos' = reverse . map (`mod` 10) . takeWhile (/=0) . iterate (`div`10)

--O bien:
digitos'' n = reverse $ map (`mod` 10) $ takeWhile (/=0) $ iterate (`div`10) n

  
