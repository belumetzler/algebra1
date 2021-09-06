--recursión

fib :: Integer -> Integer
fib n | n == 0 = 0
      | n == 1 = 1
      | n < 0 = undefined
      |otherwise = fib(n-1) + fib(n-2) 

parteEntera :: Float -> Integer
parteEntera n|n < 0 = undefined
             | n < 1 = 0
             | n >= 1 = parteEntera(n-1) + 1

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial(n-1)

esPar :: Int -> Bool
esPar n| n == 0 = True
       | n == 1 = False
       | otherwise = esPar(n-2)


multiploDe3 :: Int -> Bool
multiploDe3 n| n < 0 = undefined 
          |n == 0 = True
          | n == 1 || n == 2 = False
          |otherwise= multiploDe3(n-3)

sumaImpares :: Int -> Int 
sumaImpares n|n == 1 = 1
             |otherwise = (sumaImpares(n-1)) + n + n - 1

medioFact :: Int -> Int
medioFact 0 = 1
medioFact 1 = 1
medioFact n = n * medioFact (n-2)



--escribir una función que determine la suma de dígitos de un número positivo
--se puede utilizar div y mod
 
cantidadDeDigitos :: Int -> Int
cantidadDeDigitos n| n < 10 = 1
                   | otherwise = cantidadDeDigitos (div n 10) + 1 

digitoNunidades :: Int -> Int -> Int 
digitoNunidades x n = mod (div x (10 ^ (n-1))) 10
                   

sumaDigitos :: Int -> Int
sumaDigitos n| n < 10 = n
             | otherwise = sumaDigitos (div n 10) + mod n 10

mismosDigitos:: Int -> Bool
mismosDigitos n| n < 10 = True
               |otherwise=  mod n 10 == mod (div n 10) 10 &&  mismosDigitos(div n 10)  

 