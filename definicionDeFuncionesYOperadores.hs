{- La definicion de una funcion tiene dos partes:
   <Declaracion del tipo>           (es opcional)
   <Declaracion del comportamiento> (es obligatoria)
-}

mas1 :: Int -> Int       -- Se lee: dado un Int devuelve un Int. OJO: Int empieza por mayuscula
mas1 x = x + 1

f :: Int -> Int -> Int   -- Dado un Int devuelve una funcion que dado otro Int devuelve un Int (tambien decimos: dados dos Int devuelve un Int)
f x y = x * y + 3 

esBisiesto :: Int -> Bool
esBisiesto x = (mod x 4 == 0) && (mod x 100 /=0) || (mod x 400 == 0)

identidad :: a -> a      -- OJO: dado que el tipo a esta en minuscula, significa que vale para cualquier tipo (es polimorfico)
identidad x = x

-- Precondicion: El caracter de entrada es una minuscula
aMayuscula :: Char -> Char
aMayuscula x = toEnum (diferencia + fromEnum x)
  where diferencia = fromEnum 'A' - fromEnum 'a'     -- Con where podemos hacer definiciones auxiliares
  
otraForma :: Char -> Char
otraForma x = let diferencia = fromEnum 'A' - fromEnum 'a'   -- let tambien nos permite hacer definiciones auxiliares
              in toEnum (diferencia + fromEnum x)
              

-- Normalmente definiremos funciones por casos, veamos algunos ejemplos

-- if-then-else
mayor :: Int -> Int -> Int
mayor x y = if x > y then x else y     -- OJO: No existe el if then sin el else. if-then-else es una expresion que siempre devuelve algo, no es una instruccion

-- Guardas. Se evaluan de arriba a abajo. Se devuelve lo que haya a la derecha de la primera guarda que se cumple
mayor' :: Int -> Int -> Int
mayor' x y
  | x > y = x
  | otherwise = y
  
signo :: Int -> Int
signo x
  | x > 0 = 1
  | x < 0 = -1
  | otherwise = 0

-- case
g :: Int -> Int -> Int -> Int
g x y z = case z of
              0 -> x*y
              1 -> x+y
              otherwise -> x-y              
              
-- Patrones. Se comprueban las alternativas de arriba a abajo. Se aplica la primera regla que ajuste patron con los datos que nos pasen.
g' :: Int -> Int -> Int -> Int
g' x y 0 = x*y
g' x y 1 = x+y
g' x y z = x-y              

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)

fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

repetir :: Int -> a -> [a]                   -- Dado un entero y un elemento de cualquier tipo a, devuelve una lista de cosas de ese tipo a
repetir 0 x = []                             -- Esta funcion esta predefinida como replicate
repetir n x = x : repetir (n-1) x

sumatorio :: [Int] -> Int                    -- Esta funcion esta predefinida como sum
sumatorio [] = 0
sumatorio (x:xs) = x + sumatorio xs          -- El patron (x:xs) representa listas no vacias, cuyo primer elemento es x y cuyo resto de la lista es xs

longitud :: [a] -> Int                       -- Esta funcion esta predefinida como length
longitud [] = 0
longitud (_:xs) = 1 + longitud xs            -- El patron _ se puede usar cuando no necesitamos hacer referencia a ese valor en la parte de la derecha

alReves :: [a] -> [a]                        -- Esta funcion esta predefinida (mejor hecha, como ya veremos) como reverse
alReves [] = []
alReves (x:xs) = (alReves xs) ++ [x]         -- Para añadir un elemento al final de una lista necesitamos concatenar una lista unitaria

coger :: Int -> [a] -> [a]                   -- Esta funcion esta predefinida como take
coger 0 _ = []
coger n [] = []
coger n (x:xs) = x : coger (n-1) xs 

saltar :: Int -> [a] -> [a]                  -- Esta funcion esta predefinida como drop
saltar 0 xs = xs
saltar _ [] = []
saltar n (x:xs) = saltar (n-1) xs

partir :: Int -> [a] -> ([a],[a])            -- Si quiero devolver dos cosas, devuelvo una tupla con las dos cosas que quiera devolver
partir 0 xs = ([],xs)                        -- Esta funcion esta predefinida como splitAt
partir _ [] = ([],[])
partir n (x:xs) = (x:iz,dr)
  where (iz,dr) = partir (n-1) xs
  
cremallera :: [a] -> [b] -> [(a,b)]          -- Esta funcion esta predefinida como zip
cremallera (x:xs) (y:ys) = (x,y) : cremallera xs ys
cremallera _ _ = []

anticremallera :: [(a,b)] -> ([a],[b])       -- Esta funcion esta predefinida como unzip 
anticremallera [] = ([],[])
anticremallera ((a,b):xs) = (a:as,b:bs)
  where (as,bs) = anticremallera xs
  
insertSort :: [Int] -> [Int]
insertSort [] = []
insertSort (x:xs) = inserta x (insertSort xs)

inserta :: Int -> [Int] -> [Int]
inserta x [] = [x]
inserta x (y:ys)
  | x <= y = x : y : ys
  | otherwise = y : inserta x ys
  
{- Realmente, los tipos que hemos ido poniendo hasta ahora son mas restrictivos de lo que podrian ser. Por ejemplo,
ordenar enteros se hace exactamente igual que ordenar caracteres, o que ordenar cualquier tipo que tenga una relacion
de orden. Para poder tener funciones que valgan para muchos tipos (pero no para todos) tenemos las clases de tipos.
Una clase de tipos engloba todos los tipos que comparten una serie de funciones y/o operadores. Por ejemplo, la funcion
sumatorio podria valer para cualquier tipo numerico (incluyendo los tipos numericos que definamos nosotros) y la
funcion insertSort podria valer para cualquier tipo que tenga orden (incluidos tipos que definamos nosotros). Para
ello habria que poner el tipo adecuado... o no poner ninguno y que lo infiera Haskell. -}

sumatorio' :: Num a => [a] -> a         -- Para cualquier tipo a que sea numerico, dada una lista de cosas de tipo a, devuelve algo de tipo a
sumatorio' [] = 0
sumatorio' (x:xs) = x + sumatorio' xs   

insertSort' :: Ord a => [a] -> [a]      -- Para cualquier tipo a que tenga orden, dada una lista de cosas de tipo a, devuelve una lista de cosas de tipo a
insertSort' [] = []
insertSort' (x:xs) = inserta' x (insertSort' xs)

inserta' :: Ord a => a -> [a] -> [a] 
inserta' x [] = [x]
inserta' x (y:ys)
  | x <= y = x : y : ys
  | otherwise = y : inserta' x ys


{- 
   La definicion de un operador infijo tiene tres partes:
   <Declaracion de asociatividad y nivel de prioridad>   (es opcional: si no se indica nada, se asume infixl 9)
   <Declaracion del tipo>           (es opcional)
   <Declaracion del comportamiento> (es obligatoria)
-}

infixr 5 -+-
(-+-) :: Int -> Int -> Int
x -+- y = x*y-4              

infixl 3 &+
(&+) :: Int -> Int -> Int
a &+ 0 = a + 7
a &+ n = a * n

infixr 6 /+/                          -- Inventamos un operador para sumar dos vectores
(/+/) :: [Int] -> [Int] -> [Int]
(x:xs) /+/ (y:ys) = x+y : xs/+/ys
_ /+/ _ = []

