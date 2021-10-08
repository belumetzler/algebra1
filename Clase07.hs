type Set a = [a]


pertenece :: (Eq a) => a -> Set a -> Bool
pertenece n l | l == [] = False 
              | head l == n = True
              |otherwise = pertenece n (tail l)

vacio :: Set Int
vacio = []

agregar ::(Eq a) =>  a -> Set a -> Set a
agregar x c |pertenece x c = c 
            |otherwise = (x:c)
{--
agregarT :: (Int, Int) -> Set (Int,Int) -> Set (Int,Int)
agregarT x c |pertenece x c = c 
            |otherwise = x:c
--}
incluido :: Set Int -> Set Int -> Bool
incluido [] c =  True
incluido (x:xs) c = pertenece x c && incluido xs c

cardinal :: Set Int -> Int 
cardinal [] = 0
cardinal (x:xs) = 1 + cardinal xs

iguales :: Set Int -> Set Int -> Bool
iguales c1 c2 = incluido c1 c2 && incluido c2 c1

numApariciones :: Int -> Set Int -> Int 
numApariciones n xs| not(pertenece n xs)= 0
                   |n == head xs = 1 + numApariciones n (tail xs)
                   |otherwise = numApariciones n (tail xs)


eliminarRepetidos :: Set Int -> Set Int 
eliminarRepetidos [] = []
eliminarRepetidos (x:xs) | numApariciones x (x:xs) > 1 = eliminarRepetidos xs
                         | otherwise = agregar x (eliminarRepetidos xs)

union::  Set Int -> Set Int -> Set Int
union a b = eliminarRepetidos( a ++ b)


unionC :: Set (Set Int) -> Set (Set Int) -> Set (Set Int) 
unionC [] c = c
unionC (x:xs) c | perteneceC x c = unionC xs c 
                | otherwise = x:unionC xs c

interseccion::  Set Int -> Set Int -> Set Int
interseccion [] b = []
interseccion (a:xa) b | pertenece a b =  agregar a (interseccion xa b)
                      | otherwise = interseccion xa b

diferencia::  Set Int -> Set Int -> Set Int
diferencia [] b = []
diferencia (a:xa) b | pertenece  a b =  diferencia xa b
                    | otherwise = agregar a (diferencia xa b)

diferenciaSimetrica::  Set Int -> Set Int -> Set Int
diferenciaSimetrica a b = diferencia (union a b) (interseccion a b)


perteneceC :: Set Int -> Set(Set Int) -> Bool
perteneceC xs [] = False 
perteneceC xs (ys:yss) = iguales xs ys || perteneceC xs yss



agregarC :: Set Int -> Set (Set Int) -> Set (Set Int)
agregarC xs xss |perteneceC xs xss = xss 
                |otherwise = xs:xss


agregarATodos :: Int -> Set (Set Int) -> Set (Set Int)
agregarATodos x [] = []
agregarATodos x (c:cs) = agregarC (agregar x c) (agregarATodos x cs)



partes :: Set Int -> Set (Set Int)
partes [] = [[]]
partes (x:xs) = unionC (partes xs) (agregarATodos x (partes xs))

partesN :: Int -> Set (Set Int)
partesN 0 = [[]]
partesN n = unionC (partesN (n-1)) (agregarATodos n (partesN (n-1)))


productoCartesiano1 :: Set Int -> Set Int -> Set (Int, Int)
productoCartesiano1 (a:xa) [] = []
productoCartesiano1 (a:xa) (b:xb) = (agregar (a,b) (productoCartesiano1 (a:xa) xb)) 

productoCartesiano :: Set Int -> Set Int -> Set (Int, Int)
productoCartesiano [] b = []
productoCartesiano a b = productoCartesiano1 a b ++ productoCartesiano (tail a) b


desemparejar :: Set (Integer, Integer) -> Set Integer
desemparejar [] = []  
desemparejar ((a,b):c) = agregar a (agregar b (desemparejar c))


emparejarConSumaN :: Integer -> Set Integer -> Set (Integer, Integer)
emparejarConSumaN n [] = []
emparejarConSumaN n (c:xc) | pertenece (n-c) (xc) = (c, n-c): (emparejarConSumaN n (xc))
                      |otherwise = emparejarConSumaN n (xc)

suma :: Set Int -> Int 
suma [] = 0
suma (x:xs) = x + suma xs


seParte :: Set Int -> Bool 
seParte c |not(even (suma c)) = False
          |otherwise= recorre (div (suma c) 2) (partes c)  

recorre :: Int -> Set (Set Int) -> Bool
recorre n c | c == [] = False
            | n == suma (head c) = True
            | otherwise = recorre n (tail c)