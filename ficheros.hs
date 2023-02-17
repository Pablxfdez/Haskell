import Char

-- Copiar el contenido de un fichero en otro

copiaFichero:: IO ()
copiaFichero = do putStr "Dame el nombre del fichero de entrada"
                  inNombre <- getLine
                  contenido <- readFile inNombre
                  putStr "Dame el nombre del fichero de salida"
                  outNombre <- getLine
                  writeFile outNombre contenido

-- Dado un fichero, poner en mayúsculas todo su contenido

aMayusculasF :: IO ()
aMayusculasF = do putStr "Dame el nombre del fichero de entrada"
                  inNombre <- getLine
                  contenido <- readFile inNombre
                  putStr "Dame el nombre del fichero de salida"
                  outNombre <- getLine
                  let salida = map toUpper contenido  -- única diferencia con el ejemplo anterior
                  writeFile outNombre salida

-- Dado un fichero, cambiar todo el orden de su contenido

alRevesF :: IO ()
alRevesF = do putStr "Dame el nombre del fichero de entrada"
              inNombre <- getLine
              contenido <- readFile inNombre
              putStr "Dame el nombre del fichero de salida"
              outNombre <- getLine
              let salida = reverse contenido  -- única diferencia con el ejemplo anterior
              writeFile outNombre salida

-- Podemos definirnos un esquema general donde pasemos como parámetro la función con la 
-- que modificar el fichero

modifF :: (String -> String ) -> IO ()
modifF f = do putStr "Dame el nombre del fichero de entrada"
              inNombre <- getLine
              contenido <- readFile inNombre
              putStr "Dame el nombre del fichero de salida"
              outNombre <- getLine
              let salida = f contenido  -- llamada a la función que modifica el contenido del fichero
              writeFile outNombre salida

-- Las funciones anteriores podrían reescribirse como:

aMayusculasF' = modifF (map toUpper)
alRevesF' = modifF reverse

-- Y alguna nueva, como poner el contenido al revés, pero por líneas:

alRevesPorLineas = modifF (unlines.reverse.lines)

{- Hasta ahora hemos estado trabajando con String, ¿qué ocurre con los números u otros tipos?
Si ponemos:
> putStr 3
va a fallar porque espera algo de tipo String. Para que funcione es necesario poner:
> putStr (show 3)

show :: Show a => a -> String

convierte cualquier valor de un tipo de la clase Show, en un String.
En lugar de poner (putStr.show) 3 podemos usar la función predefinida:

print :: Show a => a -> IO ()
print = putStrLn.show -}

-- LEER UN ENTERO 

leeInt :: IO Int -- devuelve un entero recordando que viene de E/Show
leeInt = do c <- getLine
            return (read c)
            
{-- La función read no es de E/S, así que no puede ir directamente en la notación do. 
Al llamar a la función return, se convierte en una acción de E/S 

read :: Read a => String -> a
return :: a -> IO a
--}

-- LEER UN ENTERO EN UN RANGO

leeIntEnRango:: Int -> Int -> IO Int
leeIntEnRango men may = do putStr ("Introduce el numero entre " ++ show men ++ " y " ++ show may ++ ":")
                           n <- leeInt        -- puede ser una operación E/S definida por mí
                           if (n<men) || (n>may)
                              then do putStr "Mal, repite"
                                      leeIntEnRango men may
                              else return n   -- para que sea de E/S

{- COSAS NUEVAS:
- En estos casos sí es habitual utilizar la instrucción IF-THEN-ELSE ya que
no hay posibilidad de poner guardas.
- En el caso de que tengamos que poner varias instrucciones seguidas, hay que
poner de nuevo un do, ya que cada línea permite poner una única instrucción.
- Si queremos volver a realizar la misma operación de nuevo, en lugar del WHILE
imperativo, se realiza una llamada recursiva. Al llamarse a sí misma, como es 
IO Int lo que devuelve, sería una llamada correcta. -}

-- HACER UN MENÚ

menu:: IO ()
menu = do putStrLn "1. Pasar a mayúsculas el fichero"
          putStrLn "2. Darle la vuelta al fichero "
          putStrLn "3. Terminar"
          opcion <- leeIntEnRango 1 3
          case opcion of
              1 -> do aMayusculasF
                      menu
              2 -> do alRevesF
                      menu
              otherwise -> putStrLn "Hasta luego"