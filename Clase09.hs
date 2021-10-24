module Clase09 
where
    import Clase06 
    import Clase07
 


    -- | Division de numeros naturales_0 : a `divNat ` d = a `div ` d
    divNat :: Int -> Int -> Int
    divNat a d | a < d = 0
            | otherwise = (a-d) `divNat ` d + 1
    -- | Resto de numeros naturales_0 : a `modNat ` d = a `mod ` d
    modNat :: Int -> Int -> Int
    modNat a d = a - d*(a `divNat ` d)
    -- | Modulo de numeros enteros a `modulo ` d = a `mod ` d
    modulo :: Int -> Int -> Int
    modulo a d | a >= 0 || r' == 0 = r'
            | otherwise = abs d - r'
            where r' = abs a `modNat ` abs d

    -- | Division de numeros enteros : n `dividido ` m = n `div ` m
    dividido :: Int -> Int -> Int
    dividido a d = sgq * absq --obs 1
        where absq = abs (a-r) `divNat ` ( abs d) --obs 2
              sgq = ( signum a) * ( signum d) --obs 3
              r = a `modulo ` d
    {--
    Definir la funci´on digitos :: Integer -> Integer -> [Integer] que, dados n ≥ 0 y
    b > 1, retorne su representaci´on por listas en base b.
    --}

    digitos :: Int -> Int -> [Int]
    digitos n b | n < b = [n] 
                | otherwise=(mod n b):digitos (div n b) b


    {--
    Definir la funci´on numero :: [Integer] -> Integer -> Integer que, dada la
    representaci´on por listas de n ≥ 0 en base b y la base b > 1, retorne n.
    --}

    numero :: [Int] -> Int  -> Int
    numero (l:xl) n |longitud (l:xl) == 1 = l
                    |otherwise = (ultimo xl)*(n^(longitud xl)) + numero (quitarElUltimo (l:xl)) n
    {--
    Escribir la funci´on divisores :: Int -> Set Int que dado un valor n �= 0 retorna el
    conjunto de sus divisores positivos
    --}
    divisoresA:: Int -> Int -> Set Int
    divisoresA n 1 = [1]
    divisoresA n i|(mod n i)== 0 = i: (divisoresA n (i-1))
                |otherwise = divisoresA n (i-1)
    divisores n= divisoresA n n 
    --Completar la funci´on mcdDef, definiendo las funciones restantes
    mcdDef :: Int -> Int -> Int
    mcdDef a 0 = abs a
    mcdDef 0 b = abs b
    mcdDef a b = maximo ( interseccion ( divisores a) ( divisores b))
    --Medir el tiempo que tarda mcdDef para un par de valores en 10^10 ≤ a, b ≤ 2 · 10^10
    --quiero seguir con la clase y esto no termina más xd, lo voy a cortar
    --Definir la funci´on mcd :: Int -> Int -> Int que dados a, b ∈ Z, b �= 0, calcule (a : b)
    --usando el algoritmo de Euclides.
    mcd:: Int -> Int -> Int 
    mcd a 0 = a 
    mcd 0 b = b 
    mcd a b | a> b = mcd b (mod a b)
            | otherwise = mcd a (mod b a)
    {--
    Medir el tiempo de esta funci´on y compararlo con mcdDef
    *Clase09> mcdDef 39000 403
    13
    (0.10 secs, 9,961,592 bytes)
    *Clase09> mcd 39000 403   
    13
    (0.02 secs, 68,984 bytes)
    *Clase09> mcd 3943632 403232352   
    48
    *Clase09> mcdDef 3943632 403232352
    terminará de correr cuando encontremos a a la mamá de luismi
    --}

    mcmA :: Int -> Int -> Int ->Int
    mcmA a b k| k == r = k 
             |otherwise= mcmA a b (k +1)
             where r= div (abs(a*b)) (mcd a b)

    mcm ::Int -> Int -> Int
    mcm a b | a > b = mcmA a b a 
            |otherwise= mcmA a b b 
    
    esmcdA':: Int -> Int -> Int ->(Int,Int,Int)
    esmcdA' a b i|(mod (d-i*b) a)== 0 =(d,s,i) 
                 |otherwise= esmcdA' a b (i+1)
                 where s= div (d-i*b) a 
                       d= mcd a b
    esmcd' a b = esmcdA' a b 1

    --esta es la versión del libro de tere del 9

    mcdExt :: Int -> Int -> (Int , Int , Int)
    mcdExt a b | b > a = mcdExt b a
    mcdExt a 0 = (a, 1, 0)
    mcdExt a b = (d, t, s - t * k)
                where (k, r) = (div a b, mod a b)
                      (d, s, t) = mcdExt b r
    tercero (x,y,z)= z
    segundo (x,y,z)= y

    menorSAux :: Int-> Int -> Int -> (Int,Int)
    menorSAux a b j | tercero (mcdExt a b ) -j*b' < 0 = (s,t)
                    |otherwise= menorSAux a b (j+1)
                    where t = segundo(mcdExt a b) - (j-1)*b'
                          s = tercero (mcdExt a b) + (j-1)*a'
                          b' = div b (mcd a b)
                          a'= div a (mcd a b)

-- 1.1) Hacer una función que testee si es cierto que, dados tres enteros
--    positivos a, b y c, si a | c y b | c entonces a · b | c.
--
    testearHip1 :: Int -> Int -> Int -> Bool
    testearHip1 a b c | mod c a + mod c b == 0= mod c (a*b) == 0
                      |otherwise= True


-- 1.2) Hacer una función que lo testee para todos los posibles enteros 
--    positivos menores que tres valores dados.

    testearHip2:: Int ->   Int -> Int -> Bool
    vtestearHip2 _ _ 1 = True
    testearHip2 _ 0 _ = True
    testearHip2 0 _ _ = True 
    testearHip2 a b c =testearHip2 a b c && testearHip2 (a-1) b c && testearHip2 a (b-1) c && testearHip2 a b (c-1) 


-- 1.3) Hacer una función tal que si hipótesis es falsa, devuelva un
--      trio de valores que la falsifique.

-- [0,1] 2 <---> (10)_{2} = 2 = 2^1 * 1 + 2^0 * 0
-- (20)_{10} <---> [0,2] 10

-- 2) Suponiendo la representación de números en base b descripta en el video,
--    hacer un incrementador, es decir, una función con la siguiente signatura
--      inc :: Int -> [Int] -> [Int]
--    donde inc b l es la representación en base b del siguiente del natural 
--    representado por l en la misma base.
--    Ejemplos: 
--      inc 2 [0,1] = [1,1]
--      inc 2 [1,1] = [0,0,1]

-- 3) [Opcional] Asumiendo la misma representación, definir la función
--      sumar :: Int -> [Int] -> [Int] -> [Int]
--    tal que 
--     sumar b l1 l2 = digitos ((numero l1 b) + (numero l2 b)) b
--    Ejemplo:
--     sumar 2 [0,1] [0,1] = [0,0,1]

-- 4) Dados a y b dos enteros positivos, coprimos entre sí, ¿qué valores puede tomar (2a+b : 3a-2b)?
-- 
-- 4.1) Escribir una función que calcule (2a+b : 3a-2b) dados a y b.
--
-- 4.2) Escibir una función tal que para cualquier par de enteros positivos coprimos menores que a y b dados devuelva cuáles son los distintos valores que puede tomar (2a+b : 3a-2b).
--      todosLosMCD :: Int -> Int -> [Int]









