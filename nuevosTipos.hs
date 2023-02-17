{- En Haskell tenemos dos formas de definir nuevos tipos: 

-Renombrando un tipo ya exsitente, pero sin crear uno nuevo (tipos sin�nimos)
-Creando nuevos tipos de verdad (tipos algebraicos)

El renombramiento no crea un nuevo tipo, simplemente le da un nombre m�s corto
a algo que ya exist�a, pero a todos los efectos ambos tipos siguen siendo iguales,
aunque resulte c�modo y �til tener un nombre: -}

type Corto = (Int,String,[Float])

{- El tipo anterior nos puede resultar c�modo si tenemos que usar con cierta 
frecuencia ese tipo, de modo que nos ahorraremos escritura y adem�s recordaremos
con m�s facilidad lo que representa. Por ejemplo, si queremos un tipo que 
represente un n�mero complejo podr�amos tener algo as� como: -}

type Complejo = (Double,Double)

{- que no ser�a un nuevo tipo, simplemente nos permitir�a recordar que estamos
representando los complejos como una tupla con dos reales, pero a todos los efectos
el tipo ser�a equivalente a (Double,Double). Por ejemplo, podr�amos definir la suma
como: -}

sumaComplejo :: Complejo -> Complejo -> Complejo
sumaComplejo (r1,i1) (r2,i2) = (r1+r2,i1+i2)

{- Para definir nuevos tipos de verdad, usamos data. Veamos el caso m�s sencillo,
donde queremos definir un tipo enumerado con los d�as de la semana: -}

data Dias = Lunes | Martes | Miercoles | Jueves | Viernes | Sabado | Domingo

{- Tanto el nombre del tipo como las constructoras tienen que empezar en may�scula,
y todas las constructoras tienen que ser distintas entre s� y distintas de las
del resto de tipos que se definan en el mismo m�dulo. 
Podemos hacer definiciones que usen este tipo exactamente igual que las que hacemos
para los tipos ya existentes. En particular, si queremos distinguir casos, podemos
hacerlo mediante ajuste de patrones. Por ejemplo: -}

esLaborable :: Dias -> Bool
esLaborable Sabado = False
esLaborable Domingo = False
esLaborable _ = True

{- Ahora bien, si tratamos de definir esa misma funci�n pero usando operadores 
relacionales, entonces no nos funcionar�. Es decir, si ponemos

f :: Dias -> Bool
f x = (x/=Sabado) && (x/=Domingo)

esto fallar�, porque no tenemos definida la operaci�n /= para el nuevo tipo Dias. 
De hecho, nos dir� que el tipo Dias no pertenece a la clase de tipos Eq. La soluci�n 
pasa por meter el tipo Dias dentro de la clase Eq, y ya de paso dentro del resto de 
clases b�sicas que nos interesen. La foma m�s sencilla es usando la cl�usula deriving, 
que nos permite a�adir de forma autom�tica un nuevo tipo a las clases Eq, Ord, Enum, 
Show y Read, siempre y cuando nos gusten las definiciones por defecto que genera el 
compilador. Para ello, habr�a que cambiar la definici�n anterior del tipo por la siguiente:

data Dias = Lunes | Martes | Miercoles | Jueves | Viernes | Sabado | Domingo
  deriving (Eq, Ord, Enu, Show, Read)
  
con esta nueva definici�n, ya tendr�amos disponible para el nuevo tipo todas las 
operaciones de esas cinco clases, que en particular incluyen /= por lo que la funci�n f 
anterior ya funcionar�a. 


Para definir otros nuevos tipos que no sean enumerados, tambi�n usaremos la misma idea,
pero a�adiendo la informaci�n adecuada. Por ejemplo, supongamos que queremos tener
un tipo de datos que a veces tenga un entero y a veces tenga un car�cter. Podr�amos
hacerlo de la siguiente forma: -}

data EnteroOChar = A Int | B Char
  deriving (Eq, Ord, Show, Read)
  
{- es decir, tenemos una constructora distinta para cuando vamos a meter un entero que
para cuando vamos a meter un car�cter, y podremos definir funciones exactamente igual
que en el caso anterior. Por ejemplo, si queremos hacer un sumatorio de todos los 
enteros de una lista de este tipo, podr�amos hacerlo as�: -}

sumatorio :: [EnteroOChar] -> Int
sumatorio [] = 0
sumatorio (A x : xs) = x + sumatorio xs
sumatorio (B _ : xs) = sumatorio xs

{- Realmente, existe un tipo predefinido que se llama Either que extiende el ejemplo 
anterior para cualquier pareja de tipos, es decir, su definici�n es

data Either a b = Left a | Right b

es decir, nuestro tipo EnteroOChar podr�a haberse definido como Either Int Char.


Si quisi�ramos redefinir los complejos que vimos antes, pero con un nuevo tipo realmente
y no solo con un renombramiento, podr�amos haber escrito: -}

data Compl = C Double Double
  deriving (Eq, Show, Read)
  
sumaC :: Compl -> Compl -> Compl
sumaC (C r1 i1) (C r2 i2) = C (r1+r2) (i1+i2)

{- Si seguimos avanzando un poco con la definici�n de nuevos tipos, podemos tratar tambi�n
con definiciones de tipos recursivas. Por ejemplo, supongamos que no tuvi�ramos predefinidas
las listas y supongamos que queremos definir listas de enteros. La forma de hacerlo ser�a
la siguiente: -}

data ListaInt = VaciaI | ConsI Int ListaInt
  deriving (Eq, Show, Read)

{- Es decir, una lista puede estar vac�a o no estarlo. Si no lo est�, tendr� un primer 
elemento de tipo Int y despu�s vendr� otra nueva lista de enteros.
A partir de dicho tipo, podr�amos volver a definir todas las funciones que ya conocemos
sobre listas. Por ejemplo: -}

longitud :: ListaInt -> Int
longitud VaciaI = 0
longitud (ConsI _ xs) = 1 + longitud xs

misumatorio :: ListaInt -> Int
misumatorio VaciaI = 0
misumatorio (ConsI x xs) = x + misumatorio xs

{- Al igual que pasa con las listas que est�n predefinidas, podr�amos definir listas de 
cualquier tipo, no solo de enteros. Para ello tendr�amos que tener el tipo Lista parametrizado
por un tipo cualquiera a. Es decir: -}

data Lista a = Vacia | Cons a (Lista a)
  deriving (Eq, Show, Read)
  
{- N�tese que las listas no vac�as tienen un primer elemento de tipo a y luego otra lista que
tiene que ser tambi�n de cosas de tipo a. La definici�n de funciones sobre este tipo seguir�a
haci�ndose igual, teniendo en cuenta los nuevos nombres de los constructores: -}

productorio :: Num a => Lista a -> a
productorio Vacia = 1
productorio (Cons x xs) = 1 * productorio xs

{- Podemos complicar las definiciones de tipo tanto como queramos (y como nos haga falta).
Por ejemplo, si tuvi�ramos listas en las que sabemos que alternan enteros y reales (o en general
cosas de tipo a y cosas de tipo b), podr�amos definir expl�citamente el tipo para garantizar que 
despu�s de algo de tipo a viene otra cosa de tipo b y viceversa. Por ejemplo: -}

data ListaAlterna a b =  VaciaAlterna | ConsAlterna a (ListaAlterna b a) 

{- N�tese que en la llamada recursiva se usa b a en sentido inverso, para que el siguiente valor
que se meta sea del tipo b -}

{- Al igual que definimos listas, podr�amos definir �rboles o cualquier otra estructura de 
datos que nos haga falta. Por ejemplo, un �rbol binario con elementos en los nodos internos
se definir�a as�: -}

data ArbBin a = ArbVacio | NodoBin a (ArbBin a) (ArbBin a)

{- Un �rbol que a veces tiene dos hijos y otras veces tres hijos: -}

data Arb23 a = ArbVacio23 | Nodo2 a (Arb23 a) (Arb23 a) | Nodo3 (Arb23 a) (Arb23 a) (Arb23 a)

{- Un �rbol general en el que cada nodo puede tener tantos hijos como queramos: -}

data ArbGen a = ArbvacioG | NodoG a [ArbGen a]

{- N�tese que para representar que podemos tener cualquier n�mero de hijos utilizamos una lista
de �rboles, pues dicha lista puede tener 0 elementos, 1, 2... o 3234. -}

{- Otros ejemplos de tipos recursivos podr�an ser para definir qu� es una expresi�n regular: -}

data ER = VacioER | Epsilon | Terminal Char | Eleccion ER ER | Concatenar ER ER | Asterisco ER | Parentesis ER

{- O para definir expresiones aritm�ticas de enteros y evaluarlas: -}

data Expr = Cte Int | Exp Expr Op Expr
data Op = Sum | Res | Mul | Div | Mod
  deriving (Eq, Enum, Show, Read)

eval :: Expr -> Int
eval (Cte n) = n
eval (Exp e1 op e2) = aplica op (eval e1) (eval e2)

aplica :: Op -> Int -> Int -> Int
aplica op = [(+),(-),(*),div,mod] !! (fromEnum op)