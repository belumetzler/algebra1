factorial :: Int -> Int
factorial 0 = 1
factorial n = n*(factorial (n-1)) 

eAprox ::Int -> Float 
eAprox 0 = 1
eAprox n = (1 / fromIntegral(factorial(n))) + eAprox(n-1)

e :: Float
e = eAprox 10



f :: Int -> Int -> Int 
f 0 n = 0
f n m = (f (n -1) m) + round (f2 m (fromIntegral n))

sp :: Float -> Int -> Int -> Float 
sp q n 0 = 0
sp q n m = (sp q n (m-1)) + (q^m)*(f2 n q)


sp2 :: Float -> Int -> Int -> Float 
sp2 q 0 m = 0
sp2 q n m = (sp q (n-1) m) + (q^n)*(f2 m q)

g1 :: Float -> Int -> Float
g1 i n | i > fromIntegral(n) = 0
       | n == round i = i^n
       |otherwise = i^n + g1 i (n-1)  

--  g2Aux :: Float -> Int ->  Float
--  g2Aux i n |i == 0 = 0
--            | i == fromIntegral(n) = fromIntegral(n)
--            |otherwise= i^n + g2Aux (i -1) n

-- f2 es la geometrica de base q 
f2:: Int -> Float -> Float 
f2 0 q = 0
f2 n q = q^n + f2 (n-1) q 

--g2 :: Int -> Float
--g2 n | n == 0 = 0
--     | otherwise = (f2 (n-1) (fromIntegral(n))) + (g2Aux (fromIntegral(n)) n) + (g2 (n-1))


g2Aux :: Int -> Int -> Int
g2Aux 0 n = 0
g2Aux b n = b^n + g2Aux (b-1) n

g2 :: Int -> Int
g2 0 = 0
g2 n = g2Aux n n + g2 (n-1) 

g3 :: Int -> Int
g3 n |n== 0 = 0
     |mod n 2 == 0 = 2^n + g3 (n-2)
     | otherwise = g3 (n-1) 

sonIguales:: Int -> Bool
sonIguales n | n < 10 = True
             | otherwise = (mod n 10 == mod (div n 10 ) 10) && (sonIguales (div n 10))
 

g4 :: Int -> Int
g4 n|n == 0 = -45
    |sonIguales n == True = n + g4(n-1)
    | otherwise = g4(n-1)

--ejs de clase
 
h1 :: Float -> Int -> Float
h1 a 0 = 0
h1 a n =fromIntegral(n)*(a^n) + h1 a (n-1)

h2 :: Int -> Float
h2 1 = 0
h2 n = fromIntegral(n-1) / 2    + h2 (n-1)

h3Aux :: Int -> Int
h3Aux 0 = 0
h3Aux n = (2^n) + h3Aux (n - 1)

h3 ::Int -> Float
h3 n = fromIntegral(n*(2^(n+1)-2)) - h1 2 n





