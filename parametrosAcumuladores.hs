{- A veces resulta �til definir funciones auxiliares que tengan alg�n par�metro
adicional, que por ejemplo puedan usarse como "acumuladores". Si volvemos a ver
el ejemplo de calcular la lista inversa, la definici�n -}

alReves :: [a] -> [a]
alReves [] = []
alReves (x:xs) = alReves xs ++ [x]

{- ten�a el inconveniente de que su coste era O(n^2) porque ten�a n llamadas recursivas
y en cada llamada ten�amos que recorrer O(n) elementos para hacer la concatenaci�n.
Puede mejorarse la eficiencia usando un par�metro acumulador que nos permita 
sustituir la concatenaci�n de listas por a�adir un elemento al principio de una lista: -}

alReves' :: [a] -> [a]
alReves' xs = invierte xs []

invierte :: [a] -> [a] -> [a]
invierte [] ys = ys
invierte (x:xs) ys = invierte xs (x:ys)

{- Algo parecido puede hacerse en el caso de la funci�n de Fibonacci: -}

fibonacci 0 = 1
fibonacci 1 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)

{- En este caso el coste es O(2^n) y el problema radica en que estamos repitiendo c�mputos.
Por ejemplo, para calcular fibonacci 5 llamamos al de 4 y al de 3, pero el de 4 vuelve a llamar
otra vez distinta al de 3 y al de 2. La soluci�n pasa por usar un par de par�metros acumuladores
que vayan calculando dos valores consecutivos de fibonacci, de modo que primero tengamos el de 1
y el de 0, despu�s el de 2 y el de 1, luego el de 3 y el de 2 y as� sucesivamente hasta que
lleguemos al valor que necesitamos. De esta forma pasaremos a tener un coste O(n): -}

fib n = fib' n 1 1
fib' 0 x y = y
fib' n x y = fib' (n-1) (x+y) x