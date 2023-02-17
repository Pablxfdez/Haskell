{- En Haskell tenemos dos formas de definir nuevos tipos: 

-Renombrando un tipo ya exsitente, pero sin crear uno nuevo (tipos sinónimos)
-Creando nuevos tipos de verdad (tipos algebraicos)

El renombramiento no crea un nuevo tipo, simplemente le da un nombre más corto
a algo que ya existía, pero a todos los efectos ambos tipos siguen siendo iguales,
aunque resulte cómodo y útil tener un nombre: -}

type Corto = (Int,String,[Float])

{- El tipo anterior nos puede resultar cómodo si tenemos que usar con cierta 
frecuencia ese tipo, de modo que nos ahorraremos escritura y además recordaremos
con más facilidad lo que representa. Por ejemplo, si queremos un tipo que 
represente un número complejo podríamos tener algo así como: -}

type Complejo = (Double,Double)

{- que no sería un nuevo tipo, simplemente nos permitiría recordar que estamos
representando los complejos como una tupla con dos reales, pero a todos los efectos
el tipo sería equivalente a (Double,Double). Por ejemplo, podríamos definir la suma
como: -}

sumaComplejo :: Complejo -> Complejo -> Complejo
sumaComplejo (r1,i1) (r2,i2) = (r1+r2,i1+i2)

{- Para definir nuevos tipos de verdad, usamos data. Veamos el caso más sencillo,
donde queremos definir un tipo enumerado con los días de la semana: -}

data Dias = Lunes | Martes | Miercoles | Jueves | Viernes | Sabado | Domingo

{- Tanto el nombre del tipo como las constructoras tienen que empezar en mayúscula,
y todas las constructoras tienen que ser distintas entre sí y distintas de las
del resto de tipos que se definan en el mismo módulo. 
Podemos hacer definiciones que usen este tipo exactamente igual que las que hacemos
para los tipos ya existentes. En particular, si queremos distinguir casos, podemos
hacerlo mediante ajuste de patrones. Por ejemplo: -}

esLaborable :: Dias -> Bool
esLaborable Sabado = False
esLaborable Domingo = False
esLaborable _ = True

{- Ahora bien, si tratamos de definir esa misma función pero usando operadores 
relacionales, entonces no nos funcionará. Es decir, si ponemos

f :: Dias -> Bool
f x = (x/=Sabado) && (x/=Domingo)

esto fallará, porque no tenemos definida la operación /= para el nuevo tipo Dias. 
De hecho, nos dirá que el tipo Dias no pertenece a la clase de tipos Eq. La solución 
pasa por meter el tipo Dias dentro de la clase Eq, y ya de paso dentro del resto de 
clases básicas que nos interesen. La foma más sencilla es usando la cláusula deriving, 
que nos permite añadir de forma automática un nuevo tipo a las clases Eq, Ord, Enum, 
Show y Read, siempre y cuando nos gusten las definiciones por defecto que genera el 
compilador. Para ello, habría que cambiar la definición anterior del tipo por la siguiente:

data Dias = Lunes | Martes | Miercoles | Jueves | Viernes | Sabado | Domingo
  deriving (Eq, Ord, Enu, Show, Read)
  
con esta nueva definición, ya tendríamos disponible para el nuevo tipo todas las 
operaciones de esas cinco clases, que en particular incluyen /= por lo que la función f 
anterior ya funcionaría. 


Para definir otros nuevos tipos que no sean enumerados, también usaremos la misma idea,
pero añadiendo la información adecuada. Por ejemplo, supongamos que queremos tener
un tipo de datos que a veces tenga un entero y a veces tenga un carácter. Podríamos
hacerlo de la siguiente forma: -}

data EnteroOChar = A Int | B Char
  deriving (Eq, Ord, Show, Read)
  
{- es decir, tenemos una constructora distinta para cuando vamos a meter un entero que
para cuando vamos a meter un carácter, y podremos definir funciones exactamente igual
que en el caso anterior. Por ejemplo, si queremos hacer un sumatorio de todos los 
enteros de una lista de este tipo, podríamos hacerlo así: -}

sumatorio :: [EnteroOChar] -> Int
sumatorio [] = 0
sumatorio (A x : xs) = x + sumatorio xs
sumatorio (B _ : xs) = sumatorio xs

{- Realmente, existe un tipo predefinido que se llama Either que extiende el ejemplo 
anterior para cualquier pareja de tipos, es decir, su definición es

data Either a b = Left a | Right b

es decir, nuestro tipo EnteroOChar podría haberse definido como Either Int Char.


Si quisiéramos redefinir los complejos que vimos antes, pero con un nuevo tipo realmente
y no solo con un renombramiento, podríamos haber escrito: -}

data Compl = C Double Double
  deriving (Eq, Show, Read)
  
sumaC :: Compl -> Compl -> Compl
sumaC (C r1 i1) (C r2 i2) = C (r1+r2) (i1+i2)

{- Si seguimos avanzando un poco con la definición de nuevos tipos, podemos tratar también
con definiciones de tipos recursivas. Por ejemplo, supongamos que no tuviéramos predefinidas
las listas y supongamos que queremos definir listas de enteros. La forma de hacerlo sería
la siguiente: -}

data ListaInt = VaciaI | ConsI Int ListaInt
  deriving (Eq, Show, Read)

{- Es decir, una lista puede estar vacía o no estarlo. Si no lo está, tendrá un primer 
elemento de tipo Int y después vendrá otra nueva lista de enteros.
A partir de dicho tipo, podríamos volver a definir todas las funciones que ya conocemos
sobre listas. Por ejemplo: -}

longitud :: ListaInt -> Int
longitud VaciaI = 0
longitud (ConsI _ xs) = 1 + longitud xs

misumatorio :: ListaInt -> Int
misumatorio VaciaI = 0
misumatorio (ConsI x xs) = x + misumatorio xs

{- Al igual que pasa con las listas que están predefinidas, podríamos definir listas de 
cualquier tipo, no solo de enteros. Para ello tendríamos que tener el tipo Lista parametrizado
por un tipo cualquiera a. Es decir: -}

data Lista a = Vacia | Cons a (Lista a)
  deriving (Eq, Show, Read)
  
{- Nótese que las listas no vacías tienen un primer elemento de tipo a y luego otra lista que
tiene que ser también de cosas de tipo a. La definición de funciones sobre este tipo seguiría
haciéndose igual, teniendo en cuenta los nuevos nombres de los constructores: -}

productorio :: Num a => Lista a -> a
productorio Vacia = 1
productorio (Cons x xs) = 1 * productorio xs

{- Podemos complicar las definiciones de tipo tanto como queramos (y como nos haga falta).
Por ejemplo, si tuviéramos listas en las que sabemos que alternan enteros y reales (o en general
cosas de tipo a y cosas de tipo b), podríamos definir explícitamente el tipo para garantizar que 
después de algo de tipo a viene otra cosa de tipo b y viceversa. Por ejemplo: -}

data ListaAlterna a b =  VaciaAlterna | ConsAlterna a (ListaAlterna b a) 

{- Nótese que en la llamada recursiva se usa b a en sentido inverso, para que el siguiente valor
que se meta sea del tipo b -}

{- Al igual que definimos listas, podríamos definir árboles o cualquier otra estructura de 
datos que nos haga falta. Por ejemplo, un árbol binario con elementos en los nodos internos
se definiría así: -}

data ArbBin a = ArbVacio | NodoBin a (ArbBin a) (ArbBin a)

{- Un árbol que a veces tiene dos hijos y otras veces tres hijos: -}

data Arb23 a = ArbVacio23 | Nodo2 a (Arb23 a) (Arb23 a) | Nodo3 (Arb23 a) (Arb23 a) (Arb23 a)

{- Un árbol general en el que cada nodo puede tener tantos hijos como queramos: -}

data ArbGen a = ArbvacioG | NodoG a [ArbGen a]

{- Nótese que para representar que podemos tener cualquier número de hijos utilizamos una lista
de árboles, pues dicha lista puede tener 0 elementos, 1, 2... o 3234. -}

{- Otros ejemplos de tipos recursivos podrían ser para definir qué es una expresión regular: -}

data ER = VacioER | Epsilon | Terminal Char | Eleccion ER ER | Concatenar ER ER | Asterisco ER | Parentesis ER

{- O para definir expresiones aritméticas de enteros y evaluarlas: -}

data Expr = Cte Int | Exp Expr Op Expr
data Op = Sum | Res | Mul | Div | Mod
  deriving (Eq, Enum, Show, Read)

eval :: Expr -> Int
eval (Cte n) = n
eval (Exp e1 op e2) = aplica op (eval e1) (eval e2)

aplica :: Op -> Int -> Int -> Int
aplica op = [(+),(-),(*),div,mod] !! (fromEnum op)