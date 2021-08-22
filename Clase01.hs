f 0 = 1
f n = 0

 

g x y z = x + y+ z*z

doble x = 2*x

suma x y = x+y

normaVectorial x1 x2 = sqrt(x1^2 + x2^2) 

funcionConstante8 x = 8

signo  0 = 0
signo  n| n> 0 = 1
        | otherwise = -1

maximo :: Int  -> Int -> Int
maximo n1 n2 | n1 > n2 = n1
             |otherwise = n2

maximoRac :: Float -> Float -> Float
maximoRac x y | x >= y = x
              | otherwise = y

esMayorA9 :: Int -> Bool
esMayorA9 n | n > 9 = True
            | otherwise = False


esPar n | mod n 2 == 0 = True
        | otherwise = False

esImpar :: Int -> Bool
esImpar n = not ( esPar n)

f1 n | n >=3 = 5

f2 n | n >=3 =5
     | n <=1 = 8 

f3 n | n >= 3 = 5
     | n==2 = undefined
     |otherwise=8

f4 n | n >= 3 = 5
     | n <= 9 = 7

f5 n | n <= 9 = 7
     | n >= 3 = 5   

cantidadDeSoluciones b c | d > 0 =2
                         | d== 0 = 1
                         | otherwise = 0
                         where d = b**2 -4*c


--tarea

absoluto :: Int -> Int
absoluto x | x >= 0 = x
           | otherwise = (-1)*x

maximoabsoluto :: Int ->Int -> Int
maximoabsoluto x y | absoluto x > absoluto y = absoluto x
                   | otherwise = absoluto y

--sin pattern matching
algunoEs0 :: Float -> Float -> Bool
algunoEs0 x y = x == 0 || y == 0 


-- con pattern matching

algunoEs0dos  :: Float -> Float -> Bool
algunoEs0dos 0 _ = True
algunoEs0dos _ 0 = True
algunoEs0dos _ _ = False

--sin pattern matching

ambosSon0Uno :: Float -> Float -> Bool
ambosSon0Uno x y = x == 0 && y == 0 


--con
ambosSon0Dos :: Float -> Float -> Bool
ambosSon0Dos 0 0 = True
ambosSon0Dos _ _ = False

esMultiploDe :: Int -> Int -> Bool
esMultiploDe x y =  mod x y == 0 


digitoUnidades :: Int -> Int 
digitoUnidades x =  mod x 10

digitoDecenas :: Int -> Int
digitoDecenas x = div (mod x 100 - mod x 10) 10
