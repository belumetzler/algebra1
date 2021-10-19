import GHC.LanguageExtensions (Extension(FlexibleInstances))
import Data.IntSet (Key)
--funciones de otras clases
esPar :: Int -> Bool
esPar n| n == 0 = True
       | n == 1 = False
       | otherwise = esPar(n-2)

--listas
sumatoria:: [Int] -> Int
sumatoria a  | a == [] = 0
               |otherwise = head a +  sumatoria (tail a)

longitud :: [Int] -> Int 
longitud l | l== []= 0
           | otherwise= 1 + longitud( tail l )

pertenece :: Int -> [Int] -> Bool
pertenece n l | l == [] = False 
              | head l == n = True
              |otherwise = pertenece n (tail l)

primerMultiploDe45345::[Int]-> Int
primerMultiploDe45345 l | l == []= 0
                 | mod  (head l) 45345 == 0 = head l
                 |otherwise= primerMultiploDe45345 (tail l)

sumatoriaPM :: [Int] -> Int 
sumatoriaPM []= 0
sumatoriaPM (x:xs) = x + sumatoria  xs 

longitudPM:: [Int] -> Int 
longitudPM [] = 0
longitudPM (x:xs) = 1 + longitud xs

pertenecePM :: Int -> [Int] -> Bool
pertenecePM n [] = False
pertenecePM n l = n == (head l) || pertenece n (tail l) 

productoria :: [Int] -> Int 
productoria [] = 1
productoria l = (head l) * productoria (tail l)

sumarN :: Int -> [Int] -> [Int]
sumarN n [] = []
sumarN n xs = (head xs + n) :( sumarN n (tail xs) ) 

sumarElPrimero :: [Int] -> [Int]
sumarElPrimero xs = sumarN (head xs) xs

ultimoA:: [Int] -> [Int]
ultimoA xs | longitud xs == 1 = xs
           | otherwise = ultimoA (tail xs)



sumarElUltimo :: [Int] -> [Int]
sumarElUltimo xs = sumarN (head (ultimoA xs)) xs

pares :: [Int] -> [Int]
pares []= []
pares xs | esPar (head xs ) = head(xs) : pares (tail xs)
         |otherwise = pares (tail xs)



quitarTodas :: Int -> [Int] -> [Int]
quitarTodas n xs| xs == [] = []
                | head xs == n = quitarTodas n (tail xs)
                |otherwise = head xs : quitarTodas n (tail xs)

primerposicionA n xs k| not(pertenece n xs) = -1
                      | n == head xs = k
                      | otherwise = primerposicionA n (tail xs) (k+1)

primerposicion n xs = primerposicionA n xs 1

quitar :: Int -> [Int] -> [Int]
quitar n xs |1 <  primerposicion n xs = head xs : quitar n (tail xs) 
             |1 == primerposicion n xs = tail xs
             |otherwise = xs




numApariciones :: Int -> [Int] -> Int 
numApariciones n xs| not(pertenece n xs)= 0
                   |n == head xs = 1 + numApariciones n (tail xs)
                   |otherwise = numApariciones n (tail xs)

hayRepetidos :: [Int] -> Bool
hayRepetidos xs|xs== []=False
               |numApariciones (head xs) xs > 1 = True
               |otherwise = hayRepetidos (tail xs)
{--
eliminarRepetidosAlFinal :: [Int] -> [Int] que deja en la lista la primera aparici´on
de cada elemento, eliminando las repeticiones adicionales.
--}

eliminarRepetidosAlFinalA::[Int]-> [Int] -> [Int]
eliminarRepetidosAlFinalA [] c = c
eliminarRepetidosAlFinalA (l:xl) c | pertenece l c = eliminarRepetidosAlFinalA xl c 
                                   |otherwise = eliminarRepetidosAlFinalA xl ( c ++ [l])

eliminarRepetidosAlFinal l  = eliminarRepetidosAlFinalA l []

{--
10 eliminarRepetidosAlInicio :: [Int] -> [Int] que deja en la lista la ´ultima aparici´on
de cada elemento, eliminando las repeticiones adicionales.

--}
eliminarRepetidosAlInicio :: [Int] -> [Int]
eliminarRepetidosAlInicio [] = []
eliminarRepetidosAlInicio (l:xl) | numApariciones l (l:xl) == 1 = l:(eliminarRepetidosAlInicio xl)
                                |otherwise =eliminarRepetidosAlInicio xl

--11 maximo :: [Int] -> Int que calcula el m´aximo elemento de una lista no vac´ıa.


maximoA ::Int -> [Int] -> Int
maximoA n [] = n
maximoA n (l:xl) | n <= l = maximoA l xl 
                 |otherwise = maximoA n xl

maximo l = maximoA (head l) l

minimoA ::Int -> [Int] -> Int
minimoA n [] = n
minimoA n (l:xl) | n >= l = minimoA l xl 
                 |otherwise = minimoA n xl

minimo l = minimoA (head l) l

--ordenar :: [Int] -> [Int] que ordena los elementos de forma creciente.


ordenar :: [Int] -> [Int]
ordenar [] = []
ordenar (l:xl)= (minimo (l:xl)):ordenar ( quitar (minimo(l:xl)) (l:xl) )

--13 reverso :: [Int] -> [Int] que dada una lista invierte su orde

quitarElUltimo :: [a] -> [a]
quitarElUltimo (l:xl)| length (l:xl) > 1 = l:(quitarElUltimo xl)
                     |otherwise = []


reverso :: [Int] -> [Int]
reverso (l:xl) |length (l:xl) > 1 =(ultimo xl):(reverso (quitarElUltimo (l:xl)))
               |otherwise = (l:xl)

ultimo l = head (ultimoA l)



-- concatenar :: [Int] -> [Int] -> [Int] que devuelve la concatenaci´on de la primera
--lista con la segunda. Ejemplo concatenar [1,2,3] [4,5,6] [1,2,3,4,5,6],
--concatenar [] [4,5,6] --->[4,5,6]. Esta operaci´on est´a en el prelude y se escribe como
--(++).

concatenar :: [Int] -> [Int] -> [Int]
concatenar (a:xa) (b:xb)=a:(concatenar xa (b:xb))
concatenar [] (b:xb) = b:(concatenar [] xb)
concatenar [] [] = []
