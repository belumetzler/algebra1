module Clase10 
where
import Clase05 
import Clase09

--solucionEc resuelve una ecuación lineal de congruencia 
ecEquivalente :: (Int, Int, Int) -> (Int, Int, Int)
ecEquivalente (a,b,m)|(mod b d) /= 0 = undefined
                     |otherwise = (div a d, div b d, div m d)
                     where d = mcd a m

solucionEcConPropAdic :: (Int, Int, Int) -> (Int, Int)
solucionEcConPropAdic (a, b, m) = (mod (s*b) m, m)
                                where (d,s,t) = mcdE a m

solucionEc :: (Int, Int, Int) -> (Int, Int)
solucionEc e = solucionEcConPropAdic (ecEquivalente e)

-----
sistemaSimplifEquiv:: [(Int, Int, Int)] -> [(Int, Int)]
sistemaSimplifEquiv []= []
sistemaSimplifEquiv (e:es)= (solucionEc e):(sistemaSimplifEquiv es)
-------

modulos :: [(Int,Int)]-> [Int]
modulos []= []
modulos ((r,m):es)= m:(modulos es)

mayorModulo :: [(Int, Int)]-> Int 
mayorModulo sist = maximum (modulos sist)

cotaParaPrimoMaloDesde:: [(Int, Int)] -> Int -> Int 
cotaParaPrimoMaloDesde sist n | nEsimoPrimo (n+1) > (mayorModulo sist)= n 
                              |otherwise = cotaParaPrimoMaloDesde sist (n+1)

cotaParaPrimoMalo :: [(Int, Int)] -> Int 
cotaParaPrimoMalo sist = cotaParaPrimoMaloDesde sist 1 

cantidadMultiplos :: [Int] -> Int -> Int 
cantidadMultiplos [] _ = 0
cantidadMultiplos (m:ms) n | mod m (nEsimoPrimo n) == 0 = 1 + cantidadMultiplos ms n 
                           |otherwise = cantidadMultiplos ms n 

esPrimoMalo :: [(Int, Int)] -> Int -> Bool 
esPrimoMalo sist n = cantidadMultiplos (modulos sist) n >= 2 

todosLosPrimosMalosHasta :: [(Int, Int)] -> Int -> [Int]
todosLosPrimosMalosHasta _ 0 = []
todosLosPrimosMalosHasta sist n| esPrimoMalo sist n = (nEsimoPrimo n):(todosLosPrimosMalosHasta sist (n-1))
                               |otherwise= todosLosPrimosMalosHasta sist (n-1)

todosLosPrimosMalos ::[(Int, Int)] -> [Int]
todosLosPrimosMalos [] = []
todosLosPrimosMalos sist = todosLosPrimosMalosHasta sist (cotaParaPrimoMalo sist)
----
----para esta primera función supongo que m2 >= m1

solucDosEcPotenciasPrimoOrd :: (Int, Int) -> (Int, Int) -> (Int, Int) 
solucDosEcPotenciasPrimoOrd (r1, m1) (r2, m2)|mod (r2 - r1) m1 == 0 = (r2, m2)
                                             |otherwise= undefined 

solucDosEcPotenciasPrimo:: (Int, Int) -> (Int, Int) -> (Int,Int) 
solucDosEcPotenciasPrimo (r1, m1) (r2,m2) | m1<= m2 = solucDosEcPotenciasPrimoOrd (r1,m1) (r2,m2)
                                          |otherwise = solucDosEcPotenciasPrimoOrd (r2,m2) (r1,m1)

solucSistemaPotenciasPrimo :: [(Int, Int)] -> (Int, Int)
solucSistemaPotenciasPrimo [e] = e 
solucSistemaPotenciasPrimo (e1:e2:es) = solucSistemaPotenciasPrimo ((solucDosEcPotenciasPrimo e1 e2):es)

-----
quePotenciaLoDivide :: Int -> Int -> Int
quePotenciaLoDivide m p | m `mod` p == 0 = 1 + quePotenciaLoDivide (m `div` p)p 
                        | otherwise = 0
---
desdoblarSistemaEnFcionPrimo :: [(Int, Int)] -> Int ->([(Int, Int)], [(Int, Int)])
desdoblarSistemaEnFcionPrimo [] _ = ([],[])
desdoblarSistemaEnFcionPrimo ((r,m):es) p| k == 0 = (pri, (r,m):seg)
                                         |m == p^k = ((r,m):pri,seg)
                                         |otherwise = ((mod r (p^k), p^k):pri, (mod r (div m (p^k)),div m (p^k)):seg)
                        where (pri,seg)= desdoblarSistemaEnFcionPrimo es p 
                              k = quePotenciaLoDivide m p 

sistemaEquivSinPrimosMalosAux :: [(Int,Int)] -> [Int] -> [(Int, Int)]
sistemaEquivSinPrimosMalosAux sist [] = sist 
sistemaEquivSinPrimosMalosAux sist (p:ps) = (solucSistemaPotenciasPrimo pri):(sistemaEquivSinPrimosMalosAux seg ps) 
                           where (pri,seg)= desdoblarSistemaEnFcionPrimo sist p

sistemaEquivSinPrimosMalos ::[(Int,Int)] -> [(Int,Int)]
sistemaEquivSinPrimosMalos sist = sistemaEquivSinPrimosMalosAux sist (todosLosPrimosMalos sist)

solucSistemaModCoprimos :: [(Int,Int)] -> (Int,Int)
solucSistemaModCoprimos [e] = e 
solucSistemaModCoprimos ((r1,m1):(r2,m2):es)= solucSistemaModCoprimos ((r,m1*m2):es)
                                    where (d,s,t)= mcdE m1 m2 
                                          r= mod (r1*t*m2 + r2*s*m1) (m1*m2)

solucSistema :: [(Int,Int,Int)] -> (Int, Int) 
solucSistema sist = solucSistemaModCoprimos (sistemaEquivSinPrimosMalos(sistemaSimplifEquiv sist))    


--ej1 

cadaEcTieneSoluc :: [(Int, Int, Int)] -> Bool
cadaEcTieneSoluc []= True
cadaEcTieneSoluc ((a,b,c):xs)= mod b (mcd a c) == 0 && cadaEcTieneSoluc xs

--ej2
tieneSolucDosEcPotenciasPrimoOrd :: (Int, Int) -> (Int, Int) -> Bool
tieneSolucDosEcPotenciasPrimoOrd (r1, m1) (r2, m2)|mod (r2 - r1) m1 == 0 = True
                                             |otherwise= False

tieneSolucDosEcPotenciasPrimo:: (Int, Int) -> (Int, Int) -> Bool
tieneSolucDosEcPotenciasPrimo (r1, m1) (r2,m2) | m1<= m2 = tieneSolucDosEcPotenciasPrimoOrd (r1,m1) (r2,m2)
                                          |otherwise = tieneSolucDosEcPotenciasPrimoOrd (r2,m2) (r1,m1)

tieneSolucSistemaPotenciasPrimo :: [(Int, Int)] -> Bool
tieneSolucSistemaPotenciasPrimo [e] = True 
tieneSolucSistemaPotenciasPrimo (e1:e2:es) = tieneSolucDosEcPotenciasPrimo e1 e2 && tieneSolucSistemaPotenciasPrimo (e2:es)

solucSimplifA:: [(Int,Int)]-> [Int] -> Bool
solucSimplifA sist [] =True
solucSimplifA sist (l:xl) = tieneSolucSistemaPotenciasPrimo x && solucSimplifA y xl
            where (x,y) = desdoblarSistemaEnFcionPrimo sist l
solucSimplif sist =solucSimplifA sist (todosLosPrimosMalos sist)



tieneSolucion :: [(Int,Int,Int)] -> Bool 
tieneSolucion sist = cadaEcTieneSoluc sist && solucSimplif (sistemaSimplifEquiv(sist))

numPrimoDesde:: Int -> Int -> Int 
numPrimoDesde m j|m == nEsimoPrimo j = j 
                 |otherwise =numPrimoDesde m (j+1) 

numPrimo m  = numPrimoDesde m 1

dirichletA:: Int-> Int -> Int -> Int
dirichletA r m k|esPrimo (m*k + r)= m*k + r
                |otherwise= dirichletA r m (k+1)
 
dirichlet r m = dirichletA r m 0
 


