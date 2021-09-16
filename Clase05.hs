--Recursión con funciones auxiliares

factorial:: Int -> Int
factorial 0 = 1
factorial n = n*factorial (n-1)

prod :: Int -> Int -> Int
prod d h | d == h = d
         | otherwise = h * prod d (h-1)

prod' :: Int -> Int -> Int
prod' d h | d == h = h
          | otherwise = d* prod' (d+1) h

sumaDivisoresDesde :: Int -> Int -> Int
sumaDivisoresDesde d h | h == d = h
                       | mod h d == 0 = d + sumaDivisoresDesde (d + 1) h
                       | otherwise = sumaDivisoresDesde (d + 1) h

sumaDivisores :: Int -> Int
sumaDivisores h = sumaDivisoresDesde 1 h


--esto no funciona
sumaDivisoresHasta:: Int -> Int -> Int
sumaDivisoresHasta n k | k == 1 = 1
                       | mod n k == 0 = k + sumaDivisoresHasta n (k-1)
                       | otherwise = sumaDivisoresHasta n (k-1)

menorDivisorDesde :: Int -> Int -> Int
menorDivisorDesde n d | mod n d == 0 = d
                      | otherwise = menorDivisorDesde n (d+1)


menorDivisor :: Int -> Int
menorDivisor n = menorDivisorDesde n 2

esPrimo :: Int -> Bool
esPrimo 1 = False
esPrimo n = menorDivisor n == n

minimoPrimoDesde :: Int  -> Int
minimoPrimoDesde d | esPrimo (d) = d
                   | otherwise = minimoPrimoDesde (d+1)

nEsimoPrimo :: Int -> Int
nEsimoPrimo 1 = 2
nEsimoPrimo n = minimoPrimoDesde(nEsimoPrimo(n-1) + 1)

menorFactorialDesdeDesde :: Int -> Int -> Int
menorFactorialDesdeDesde n m | (factorial n) >= m = factorial n
                             | otherwise = menorFactorialDesdeDesde (n+1) m

menorFactorialDesde :: Int -> Int
menorFactorialDesde n = menorFactorialDesdeDesde 1 n

mayorFactorial :: Int -> Int -> Int
mayorFactorial n m | factorial n > m = factorial (n - 1)
                   | factorial n == m = factorial (n)
                   | otherwise = mayorFactorial (n+1) m

mayorFactHasta :: Int -> Int
mayorFactHasta m = mayorFactorial 1 m

esFactAux :: Int -> Int -> Bool
esFactAux n m| factorial n == m = True
             | factorial n > m = False
             | otherwise = esFactAux (n+1) m

esFact :: Int -> Bool
esFact n = esFactAux 1 n

fib :: Int -> Int
fib n = round ( (1/sqrt(5))*((1+sqrt(5))/2)^n - (1/sqrt(5))*((1-sqrt(5))/2)^n )

esFibAux :: Int -> Int -> Bool
esFibAux n m | n == fib m = True
             | n < fib m = False
             |otherwise = esFibAux n (m+1)

esFib :: Int -> Bool
esFib n = esFibAux n 0

sumaImPrimos:: Int -> Int -> Int
sumaImPrimos m d |m == 0 = 0
                 |esPrimo d = d + sumaImPrimos (m-1) (d+1)
                 |otherwise = sumaImPrimos m (d+1)

sumaPrimosAux :: Int -> Int -> Bool
sumaPrimosAux n d| sumaImPrimos d 0 == n = True
                 |sumaImPrimos d 0 > n = False
                 |otherwise = sumaPrimosAux n (d+1)

esSumaPrimos n = sumaPrimosAux n 0



tomaValorMaxAux :: Int -> Int -> (Bool, Int)
tomaValorMaxAux n m| n == m = (True,m)
                   | (sumaDivisores m >= sumaDivisores (m - 1)) && fst (tomaValorMaxAux n (m-1)) = (True, m)
                   | otherwise = tomaValorMaxAux n (m-1)

tomaValorMax :: Int -> Int -> Int
tomaValorMax n m = snd ( tomaValorMaxAux n m)

tomaValorMinAux :: Int -> Int -> (Bool, Int)
tomaValorMinAux n m| n == m = (False,m)
                   | m == n + 1 && sumaDivisores m < sumaDivisores n = (True, m)
                   | (sumaDivisores m <= sumaDivisores (m - 1)) &&  fst (tomaValorMinAux n (m-1)) = (True, m)
                   | otherwise = tomaValorMinAux n (m-1)

tomaValorMin n m = snd( tomaValorMinAux n m)

primosGemAux :: Num p => Int -> Int -> p
primosGemAux n d | d == n +1 = 0
                 | esPrimo(d) && esPrimo (d + 2) && ((d+2) <= n ) = 1 + primosGemAux n (d + 1)
                 |otherwise = primosGemAux n (d+1)

primosGem :: Int -> Int
primosGem n = primosGemAux n 1

proxPrimosGemA :: Int ->Int -> (Int, Int)
proxPrimosGemA n d | esPrimo d && esPrimo (d+2) && (d > n )= (d,d+2)
                   | otherwise = proxPrimosGemA n (d+1)

proxPrimosGem n = proxPrimosGemA n 1

largoSecuencia :: Int -> Int
largoSecuencia n| n == 1 = 0
                | mod n 2 == 0 = largoSecuencia(div n 2) + 1
                |otherwise = largoSecuencia (3*n +1) + 1

esEste :: Int -> Int -> Int -> Bool
esEste n d h | d == h + 1 = True
             | (largoSecuencia n >= largoSecuencia d) && esEste n (d + 1) h = True
             | otherwise = False

secuenciaMax n | esEste n 1 10000 = n 
               | otherwise = secuenciaMax (n+1)



sonCoprimosA :: Int -> Int -> Int -> Bool
sonCoprimosA n m d | d > n = True
                   | (mod n d == 0 &&  mod m d == 0)  = False
                   |otherwise = sonCoprimosA n m (d+1)

sonCoprimos :: Int -> Int -> Bool
sonCoprimos n m = sonCoprimosA n m 2