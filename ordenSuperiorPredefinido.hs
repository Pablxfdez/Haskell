{- Las funciones de orden superior nos permiten definir esquemas genéricos que se definen una
única vez y se pueden usar muchas veces. Las funciones de orden superior más habituales que
usaremos serán map, filter, zipWith, takeWhile, dropWhile, span y las familias fold y scan.
Veamos cómo se definen las más sencillas: -}

--Aplicamos una misma función a cada elmento de una lista, por ejemplo para pasar a mayúsculas,
--elevar al cuadrado cada elemento, etc.
mimap :: (a->b) -> [a] -> [b]
mimap f [] = []
mimap f (x:xs) = f x : map f xs

incrementarTodoEnUno = map (+1)
negarTodo = map not 

--Devolvemos la misma lista pero quedándonos solo con los que cumplen la propiedad que pasemos
--como primer parámetro
mifilter :: (a->Bool) -> [a] -> [a]
mifilter p [] = []
mifilter p (x:xs)
  | p x = x : mifilter p xs
  | otherwise = mifilter p xs

quedarseConLosPares = filter even
quedarseConLosPositivos = filter (>0)
  
--Generaliza zip usando una función para combinar los elementos de la primera y la segunda lista  
mizipWith :: (a->b->c) -> [a] -> [b] -> [c]  
mizipWith f (x:xs) (y:ys) = f x y : mizipWith f xs ys
mizipWith f _ _ = []

sumaVectores = zipWith (+)
sumaMatrices = zipWith sumaVectores
prodEscalar xs ys = sum (zipWith (*) xs ys)

--Generaliza take usando una función para decidir hasta cuándo hay que coger elementos de la lista
mitakeWhile :: (a->Bool) -> [a] -> [a]
mitakeWhile p [] = []
mitakeWhile p (x:xs)
  | p x = x : mitakeWhile p xs
  | otherwise = []
  
cogeUnaLinea = takeWhile (/='\n')  

--Generaliza drop usando una función para decidir hasta cuándo hay que coger elementos de la lista
midropWhile :: (a->Bool) -> [a] -> [a]
midropWhile p [] = []
midropWhile p (x:xs)
  | p x = midropWhile p xs
  | otherwise = x:xs

saltaUnaLinea = dropWhile (/='\n')  
  
--Generaliza splitAt usando una función para decidir hasta cuándo hay que coger elementos de la lista
mispan :: (a->Bool) -> [a] -> ([a],[a])
mispan p [] = ([],[])
mispan p (x:xs)
  | p x = (x:iz,dr)
  | otherwise = ([],x:xs)
  where (iz,dr) = span p xs
  
lineas :: String -> [String]  --Predefinido como lines  
lineas [] = []
lineas xs 
  | null dr = [iz]
  | otherwise = iz : lineas (tail dr)
  where (iz,dr) = span (/='\n') xs
  
  