type Set a = [a]

fact:: Int -> Int
fact 0 = 1
fact n = n*fact (n-1)

pertenece:: (Eq a) => a -> Set a -> Bool
pertenece _ [] = False
pertenece y (x:xs) = (x==y) || (pertenece y xs)

agregar:: (Eq a) => a -> Set a -> Set a
agregar x c | pertenece x c = c
            | otherwise = (x:c)

combinatorio:: Int -> Int -> Int
combinatorio n k = div (fact n) ((fact k)*(fact (n-k)))

combinatorio'::Int->Int->Int
combinatorio' n 0 = 1
combinatorio' n k | k == n = 1
                  | otherwise = (combinatorio' (n-1) (k)) + (combinatorio' (n-1) (k-1))

agregarATodos':: (Eq a)=> a -> Set (Set a) -> Set (Set a)
agregarATodos' x [] = [] 
agregarATodos' x (c:cs) = agregar (x:c) (agregarATodos' x cs)

--que dado un conjunto c y una longitud k
--genere todas las posibles listas de longitud k a partir de elementos de c.


vacio:: Set a 
vacio = []


union:: (Eq a)=>Set a -> Set a -> Set a
union [] c2 = c2 
union (x:xs) c2 | (pertenece x c2) = union xs c2
                | otherwise = union xs (x:c2)




agregarElementoAdelante :: Int -> Set [Int] -> Set [Int] 
agregarElementoAdelante x []= []
agregarElementoAdelante x (ys:yss) =agregar (x:ys) (agregarElementoAdelante x yss)

agregarElementosAListas :: Set Int -> Set [Int] -> Set [Int]
agregarElementosAListas [] _ = []
agregarElementosAListas (x:xs) c =union (agregarElementoAdelante x c) (agregarElementosAListas xs c)


variaciones :: Set Int -> Int -> Set [Int]
variaciones c 0 = [[]]
variaciones c k =agregarElementosAListas c (variaciones c (k-1))

--Permutaciones
--insertarEn :: [Int] -> Int -> Int -> [Int] que dados una lista l, un n´umero n y una
--posici´on i (contando desde 1) devuelva una lista en donde se insert´o n en la posici´on i de l
--y los elementos siguientes corridos en una posici´on

insertarEn :: [Int] -> Int -> Int -> [Int] 
insertarEn xs n i | i == 1 = n:xs
                  |otherwise= (head xs):(insertarEn (tail xs) n (i-1))
{--
permutaciones :: Set Int -> Set [Int]
que dado un conjunto de enteros, genere todas las posibles permutaciones de los n´umeros
del conjunto pasado por par´ametro
--}
insertarEnCadaPosicion :: [Int] -> Int -> Int -> Set [Int]
insertarEnCadaPosicion xs c 1 =agregar  (insertarEn xs c 1) vacio
insertarEnCadaPosicion xs c i = agregar (insertarEn xs c i) (insertarEnCadaPosicion xs c (i-1)) 


insertarEnCadaPosicionDeTodasLasListas:: Set [Int] -> Int -> Set [Int]
insertarEnCadaPosicionDeTodasLasListas [] c = []
insertarEnCadaPosicionDeTodasLasListas (xs:xss) c = union (insertarEnCadaPosicion  xs c (length xs + 1)) (insertarEnCadaPosicionDeTodasLasListas xss c)  

insertarEnPrimeraPosicionDeTodasLasListas:: Set [Int] -> Int -> Set [Int]
insertarEnPrimeraPosicionDeTodasLasListas [] c = []
insertarEnPrimeraPosicionDeTodasLasListas (xs:xss) c = union (insertarEnCadaPosicion xs c 1) (insertarEnPrimeraPosicionDeTodasLasListas xss c)

permutaciones :: Set Int -> Set [Int]
permutaciones [] = [[]]
permutaciones (c:cs) = insertarEnCadaPosicionDeTodasLasListas (permutaciones cs) c 

bolitasEnCajas :: Int -> Int -> Set [Int]
bolitasEnCajas n k = variaciones [1..k] n 

primeraNoVacia:: Int -> Int -> Set [Int]
primeraNoVacia n k =(insertarEnCadaPosicionDeTodasLasListas  (variaciones [1..k] (n-1)) 1) 
-----
estaOrdenada :: Set Int -> Bool 
estaOrdenada s |length s == 1 = True
               |otherwise= ((head s) < head(tail s)) && estaOrdenada (tail s) 

todasLasOrdenadasA ::  Set [Int] -> Set [Int]
todasLasOrdenadasA [] = []
todasLasOrdenadasA (xs:xss) |estaOrdenada xs = agregar (xs)  (todasLasOrdenadasA xss) 
                           |otherwise= todasLasOrdenadasA xss

todasLasOrdenadas n k = todasLasOrdenadasA (variaciones [1..n] k)

----Todas las sucesiones de los caracteres ’a’ y ’b’ de longitud n y m respectivamente.

insertarEn' ::[Char] -> Char -> Int -> [Char] 
insertarEn' xs n i | i == 1 = n:xs
                  |otherwise= (head xs):(insertarEn' (tail xs) n (i-1))


insertarEnCadaPosicion' :: [Char] -> Char -> Int -> Set [Char]
insertarEnCadaPosicion' xs c 1 =agregar  (insertarEn' xs c 1) vacio
insertarEnCadaPosicion' xs c i = agregar (insertarEn' xs c i) (insertarEnCadaPosicion' xs c (i-1)) 


insertarEnCadaPosicionDeTodasLasListas'::Set [Char] -> Char -> Set [Char]
insertarEnCadaPosicionDeTodasLasListas' [] c = []
insertarEnCadaPosicionDeTodasLasListas' (xs:xss) c = union (insertarEnCadaPosicion'  xs c (length xs + 1)) (insertarEnCadaPosicionDeTodasLasListas' xss c)  


permutaciones' :: Set Char -> Set [Char]
permutaciones' [] = [[]]
permutaciones' (c:cs) = insertarEnCadaPosicionDeTodasLasListas' (permutaciones' cs) c

nveces:: Int -> Char -> [Char] 
nveces 0 x = []
nveces n x =  [x]  ++ (nveces (n-1) x)

tantasvecesayb n m = (nveces n 'a') ++ (nveces m 'b') 

sucesionesDe2Caracteres n m = permutaciones' (tantasvecesayb n m)
----
tantasvecesabc n m k = (nveces n 'a') ++ (nveces m 'b') ++ (nveces k 'c')

sucesionesDe3Caracteres n m k = permutaciones' (tantasvecesabc n m k)
---
subconjuntos:: Set Int -> Int -> Set (Set Int) 
subconjuntos [] _ = [[]]
subconjuntos  _  0 = [[]]
subconjuntos (l:xl) n | n== length (l:xl) = [(l:xl)]
                      |otherwise = union (insertarEnPrimeraPosicionDeTodasLasListas (subconjuntos xl (n-1)) l) (subconjuntos xl n)