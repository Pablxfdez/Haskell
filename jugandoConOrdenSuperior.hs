-- Las siguientes funciones hacen básicamente lo mismo:

adicion (x,y) = x + y

suma x y = x + y

{- Ahora bien, en Haskell suele haber más tendencia a escribir la segunda que la primera. El
motivo es que la segunda está "currificada", es decir, recibe sus parámetros por separado.
La principal ventaja de esto es que es posible aplicar dichas funciones parcialmente. 
Si una función (u operador) tiene n parámetros y solo le pasamos m (con m < n) entonces nos
devolverá una nueva función que esperará los n-m parámetros restantes. Dicha nueva función
puede usarse de forma completamente normal, almacenarla en estructuras de datos, pasarla 
como parámetro de otra función, etc. Por ejemplo, podemos hacer

   map (suma 3) [8,2,5,4]

pero no podemos hacer
   
   map (adicion 3) [8,2,5,4]   
   
Ahora bien, jugando con funiones de orden superior puedo convertir cualquier función no-currificada
en currificada, y viceversa. Para convertir de no-currificada a currificada está predefinida la
función curry, que podríamos definir así: -}

micurry :: ((a,b) -> c) -> (a -> b -> c)
micurry f x y = f (x,y)

{- Es decir, cuando recibe la función no-currificada y recibe también por separado los dos parámetros 
que formarían la tupla de entrada de dicha función, aplica la función no-currificada sobre la tupla
que acabamos de formar.
Ahora bien, dado que micurry tiene 3 parámetros por separado, es posible aplicarla parcialmente. En
particular, si le pasamos solo su primer parámetro (que es una función) nos devolverá una nueva 
función que espera los otros dos parámetros por separado. Es decir, nos devuelve una función currificada
que espera dos parámetros.

Nótese que el tipo  
    ((a,b) -> c) -> (a -> b -> c) 
puede leerse como "dada una función no-currificada que espera una tupla, devuelve una función 
currificada que espera los dos elementos por separado". Ahora bien, dicho tipo es equivalente a 
    ((a,b) -> c) -> a -> b -> c
es decir, dada una función no currificada, y dadas otras dos entradas de tipo a y b, devuelve 
algo de tipo c.    

Así pues, si queremos aplicar parcialmente la función no-currificada adicion, podríamos escribir
  
  map (uncurry adicion 3) [8,2,5,4]
-}

{- Cuando aplicamos parcialmente un operador binario, podemos hacerlo indistintamente pasando solo
el primer parámetro o solo el segundo. Por ejemplo

  map (^2) [1,2,3,4]

devuelve los cuadrados de los elementos de la lista, mientras que 

  map (2^) [1,2,3,4]
  
devuelve cuatro potencias de 2.
Ahora bien, cuando aplicamos parcialmente una función, siempre debemos hacerlo en el orden en el
que tenemos los parámetros, es decir, solo podemos aplicar parcialmente los primeros parámetros. 
Por ejemplo, lo siguiente es correcto:

  map (take 3) ["esto","es","una","prueba"]

devuelve  ["est","es","una","pru"] pero si quisiéramos aplicar parcialmente el take con su segundo
parámetro (por ejemplo, para hacer algo así como map (take "prueba") [1,3,2,4] con el objetivo
de sacar 1, 3, 2 o 4 caracteres de la palabra prueba, entonces no funcionaría.
Ahora bien, podemos definir funciones de orden superior que dada una función con dos parámetros
nos devuelva otra función equivalente pero que espere los parámetros en el orden opuesto. En particular,
flip le da la vuelta al orden de los dos primeros parámetros: -}

miflip :: (a -> b -> c) -> (b -> a -> c)
miflip f y x = f x y

{- Nótese que lo único que hace flip es invocar a la función f, pero pasando los parámetros en el 
orden que necesita f. Gracias a esta función, podemos escribir cosas como

  map (flip take "prueba") [1,3,2,4]
  
Nótese también que la función flip también vale para funciones que tengan más de dos parámetros de
entrada. Ahora bien, lo único que haría en dichos casos es intercambiar el orden de los dos primeros.
Si queremos pasar a que el tercer parámetro sea el primero, habría que definir otra función flip3
que haga lo que queremos. -}

{- En general, podemos definir tantas funciones y operadores de orden superior como nos interese.
Algunas útiles que tenemos predefinidas son:

--Aplicación de función que asocie a derechas. Útil para escribir menos paréntesis
infixr 0 $
($) :: (a -> b) -> a -> b
f $ x = f x

--Composición de funciones
(.) :: (a -> b) -> (c -> a) -> (c -> b)
(.) f g x = f (g x)

-}
  
--Aplicar dos veces una misma función: predefinida como twice
mitwice :: (a -> a) -> a -> a
mitwice f = f . f

--Aplicar infinitas veces una misma función, devolviendo todos los resultados intermedios.
--Predefinida como iterate
miiterate :: (a -> a) -> a -> [a]
miiterate f x = x : iterate f (f x)

-- Por ejemplo, si queremos sacar la lista de dígitos de un número natural, podemos escribir:
digitos :: Int -> [Int]
digitos n= reverse (map (`mod` 10) (takeWhile (/=0) (iterate (`div`10) n)))

-- O si queremos poner menos paréntesis:
digitos' = reverse . map (`mod` 10) . takeWhile (/=0) . iterate (`div`10)

--O bien:
digitos'' n = reverse $ map (`mod` 10) $ takeWhile (/=0) $ iterate (`div`10) n

  
