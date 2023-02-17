{-Adem�s de todas las funciones de orden superior predefinidas, podemos definir las que nos interesen para
nuestro contexto concreto. Por ejemplo, si definimos �rboles binarios o �rboles generales, nos puede interesar
implementar variantes de map que funcionen con dichos �rboles en lugar de con listas. Si tenemos los tipos -}

data ArbolBin a = Vacio | Nodo a (ArbolBin a) (ArbolBin a)

--Podemos definir el map correspondiente como

mapArbol :: (a -> b) -> ArbolBin a -> ArbolBin b
mapArbol _ Vacio = Vacio
mapArbol f (Nodo x iz dr) = Nodo (f x) (mapArbol f iz) (mapArbol f dr)

--Mientras que si tenemos 

data ArbolGen a = VacioG | NodoG a [ArbolGen a]

--podemos hacer

mapAG :: (a -> b) -> ArbolGen a -> ArbolGen b
mapAG _ VacioG = VacioG
mapAG f (NodoG x hijos) = NodoG (f x) (map (mapAG f) hijos)

{-N�tese que dado que hijos es una lista de �rboles (pero lista al fin y al cabo) podemos usar el map de
las listas para aplicar a cada hijo la misma funci�n, donde la funci�n a aplicar a cada hijo es el 
map de los �rboles generales.
De hecho, para cualquier tipo de datos que definamos podemos tratar de proporcionar funciones del estilo
de map o del estilo de la familia fold. Es m�s, realmente existen clases de tipos que incluyen todos 
aquellos tipos que tienen map o que tienen fold. -}

{- Tambi�n podemos definir funciones de orden superior que se comporten como las estructuras de control
t�picas de lenguajes imperativos. Por ejemplo, un while no es nada m�s que una funci�n de orden superior
que recibe la funci�n de test para ver si el bucle debe terminar y otra funci�n que represente el cuerpo
del bucle (que dado el estado previo de las variables, devuelve el nuevo estado de las variables despu�s
de dar una vuelta por el bucle): -}

while :: (a -> Bool) -> (a -> a) -> a -> a
while test body state 
  | test state = while test body (body state)
  | otherwise = state
  
{- Y lo mismo para definir un repeat: -}

mirepeat :: (a -> Bool) -> (a -> a) -> a -> a
mirepeat test body state
  | test sig = sig
  | otherwise = mirepeat test body sig
  where sig = body state
  
{- Supongamos que queremos ver si dos funciones son iguales. En general, esto no ser� posible, porque no
es computable. Ahora bien, lo que s� que podemos hacer es pasar un mismo conjunto de tests y comprobar
si ambas funciones devuelven lo mismo o no. Si para alg�n elemento del test devuelven cosas distintas, 
entonces las funciones son distintas, mientras que si para todos los elementos del test devulven el mismo
resultado ambas funciones, entonces no podemos saber si son iguales o no (aunque nos puede dar cierta 
confianza en que tienen un comportamiento parecido). En este sentido, podemos hacer una funci�n de orden
superior que reciba dos funciones a comparar y una lista de casos de prueba: -}

tester :: Eq b => (a -> b) -> (a -> b) -> [a] -> Bool
tester f g xs = (map f xs) == (map g xs)

{- N�tese que la implementaci�n es trivial. Simplemente aplicamos la primera funci�n a cada elemento de
la lista, aplicamos la segunda funci�n a cada elemento de la lista, y comparamos si las listas de 
resultados son iguales. L�gicamente, dado que tenemos que comparar las listas de resultados, necesitamos
que dichos resultados est�n en la clase Eq, para que podamos compararlos por igualdad.
Esta funci�n de orden superior nos devuelve un booleano indicando si el comportamiento para este conjunto
de pruebas ha sido igual o no. Ahora bien, en caso de que no sea igual, no nos indica en qu� casos concretos
ha habido diferencias entre ambas funciones. Podemos modificar la definici�n anterior para que no nos
devuelva un booleano, sino la lista de datos de entrada para las que ambas funciones devuelven cosas
distintas. Si la lista es vac�a, significar�a que en todos los casos han devuelto lo mismo: -}

tester2 :: Eq b => (a -> b) -> (a -> b) -> [a] -> [a]
tester2 f g xs = map snd malos
  where comparados = zipWith (/=) (map f xs) (map g xs)
        malos = filter fst (zip comparados xs)
        
{- En comparados obtenemos una lista de booleanos donde True significa que el resultado de f y de g era
distinto, mientras que False significa que eran iguales. Para poder relacionar dichos booleanos con las
entradas a las que iban asociados, hacemos un zip entre comparados y la lista de entrada xs. Posteriormente,
como solo nos queremos quedar con los contraejemplos en los que f y g devuelven cosas distintas, hacemos
un filter mirando la primera componente, de modo que solo nos quedamos con los que en la primera componente
tienen un True. As� pues, malos ser� una lista de tuplas, donde lo que nos interesa finalmente son solo
sus segundas componentes (los casos de prueba que fallaron), por eso lo que devolvemos es: map snd malos
-}        

{- El orden superior nos permitir� definir esquemas generales que podremos usar tantas veces como queramos.
En particular, esto ser� muy �til para tratar con cualquier esquema gen�rico como los que ver�is en 
Dise�o de Algoritmos. Por ejemplo, supongamos que queremos definir un esquema general para el m�todo 
divide y vencer�s. En tal caso, tendr�amos que proporcionarle funciones para saber si el problema de entrada
es trivial o no, para resolver problemas triviales, para dividir un problema no trivial en problemas m�s
peque�os, y para combinar subsoluciones de problemas m�s peque�os en la soluci�n del problema m�s grande: -}

dv :: (a -> Bool) -> (a -> b) -> (a -> [a]) -> (a -> [b] -> b) -> a -> b
dv trivial solve split combine prob
  | trivial prob = solve prob
  | otherwise = combine prob (map (dv trivial solve split combine) (split prob))

