{- MINIMAX:

Se utiliza para juegos simétricos de dos oponentes con información completa.
Todos aquellos juegos en los que lo que es bueno para uno de los oponentes, 
es malo para el otro y viceversa. Además es necesario tener la información 
completa.
Ej.: 3 en raya, damas, ajedrez,...
No se usa en juegos como parchis, pocker, escoba, ... que son juegos de azar
en los que no se tiene información completa. -}

minimax:: Ord b => Int -> (a-> [a]) -> (a-> b) -> ([b]-> b) -> ([b] -> b) -> a -> b
minimax prof expandir evaluar peor mejor probl
   | (prof ==0) || (null siguientes) = evaluar probl
   | otherwise = mejor (map (minimax (prof -1) expandir evaluar mejor peor) siguientes)
   where siguientes = expandir probl
   
   
minimaxMain :: Ord b => Int -> (a-> [a]) -> (a-> b) -> a -> a
minimaxMain prof expandir evaluar probl
   | (prof ==0) || (null siguientes) = probl
   | otherwise = snd (maximum' sigVals) 
   where siguientes = expandir probl
         valoraciones = map (minimax (prof -1) expandir evaluar maximum minimum) siguientes)
         sigVals = zip valoraciones sigVals
 
maximum' :: Ord a => [(a,b)] -> (a,b)
{- Debe calcular el máximo de una lista de tuplas teniendo en cuenta únicamente la primera
componente de cada tupla -}