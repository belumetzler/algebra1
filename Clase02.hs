estanRelacionados :: Int -> Int -> Bool
estanRelacionados x y = (x <= 3 && y <= 3)  || (3 < x && x <= 7 && 3 < y && y <= 7) || (x>7 && y>7) 



-- es lo mismo poner prodInt ::  (Float, Float ) -> (Float, Float) -> Float
prodInt :: Num a => (a,a) -> (a,a) -> a
prodInt a b = (fst a)*(fst b) + (snd a)*(snd b)

todoMenor :: Ord a => (a,a) -> (a,a) -> Bool
todoMenor v1 v2 = ((fst v1) < (fst v2) && (snd v1) < (snd v2))


distanciaPuntos :: Floating a => (a,a) -> (a,a) -> a
--distanciaPuntos s t = sqrt ((fst s - fst t) **2 + (snd s - snd t)**2)
distanciaPuntos s t = sqrt (a **2 + b**2)
               where a = fst s - fst t 
                     b = snd s - snd t

sumaTerna :: Integral a => (a,a,a) -> a
sumaTerna (x,y,z) = x + y + z

posicPrimerPar ::  Integral a => (a,a,a) -> a
posicPrimerPar (x,y,z) | mod x 2 == 0 = 1
                         | mod y 2 == 0 = 2
                         | mod z 2 == 0 = 3
                         | otherwise = 4


crearPar x y  = (x,y)


invertir (x,y) = (y,x)