module Ficha2 where
import Data.Char

--2.a)

dobros :: [Float] -> [Float]

dobros [] = []
dobros (h:t) = 2*h : dobros t

--2.b)

numOcorre :: Char -> String -> Int

numOcorre _ [] = 0

numOcorre c (h:t) =
    if c == h
    then 1 + numOcorre c t
    else numOcorre c t

--2.c)

positivos :: [Int] -> Bool

positivos [] = True
positivos (x:xs) =
    if x > 0
    then positivos xs
    else False

--2.d)

soPos :: [Int] -> [Int]

soPos [] = []
soPos (x:xs) =
    if x > 0
    then x : soPos xs
    else soPos xs

--2.e)

somaNeg :: [Int] -> Int

somaNeg [] = 0
somaNeg (x:xs) =
    if x < 0
    then x + somaNeg xs
    else somaNeg xs

--2.f)

tresUlt :: [a] -> [a]

tresUlt [] = []

tresUlt (x:xs) =
    if length (x:xs) < 3
    then (x:xs)
    else drop (length (x:xs) - 3) (x:xs)

--2.g)

segundos :: [(a,b)] -> [b]

segundos [] = []
segundos (x:xs) = snd x : segundos xs

--2.h)

nosPrimeiros :: (Eq a) => a -> [(a,b)] -> Bool

nosPrimeiros _ [] = False
nosPrimeiros e (x:xs) =
    if e == fst x
    then True
    else nosPrimeiros e xs

--2.i)

sumTriplos :: (Num a, Num b, Num c) => [(a,b,c)] -> (a,b,c)

sumTriplos [] = (0,0,0)
sumTriplos ((x,y,z):t) =
    let (x',y',z') = sumTriplos t
    in (x + x', y + y', z + z') --estudar esta merda de funçao

--3.a)

soDigitos :: [Char] -> [Char]

soDigitos [] = []
soDigitos (c:cs)
    |isDigit c = c : soDigitos cs
    |otherwise = soDigitos cs

--3.b)

minusculas :: [Char] -> Int

minusculas [] = 0
minusculas (c:cs)
    |isLower c = 1 + minusculas cs
    |otherwise = minusculas cs

--3.c)

nums :: String -> [Int]

nums [] = []
nums (x:xs)
    |isDigit x = digitToInt x : nums xs
    |otherwise = nums xs

--4)

type Polinomio = [Monomio]
type Monomio = (Float,Int)

--4.a)

conta :: Int -> Polinomio -> Int

conta n [] = 0
conta n (x:xs)
    |n == snd x = 1 + conta n xs
    |otherwise = conta n xs

--4.b)

grau :: Polinomio -> Int

grau [] = 0
grau (x:xs) = max (snd x) (grau xs)

--4.c)

selgrau :: Int -> Polinomio -> Polinomio

selgrau _ [] = []
selgrau n (x:xs)
    |snd x == n = x : selgrau n xs
    |otherwise = selgrau n xs

--4.d)

deriv :: Polinomio -> Polinomio
deriv [] = []
deriv ((c, e):xs) = (c * fromIntegral e, e - 1) : deriv xs

--4.e)

calcula :: Float -> Polinomio -> Float

calcula _ [] = 0
calcula n ((c, e):xs) = c * n^e + calcula n xs

--4.f)

simp :: Polinomio -> Polinomio

simp [] = []
simp ((c,e):xs)
    |c == 0 = simp xs
    |otherwise = (c,e) : simp xs

--4.g)

mult :: Monomio -> Polinomio -> Polinomio

mult _ [] = []
mult (c1,e1) ((c2,e2):xs) = (c1 * c2, e1 + e2) : mult (c1,e1) xs

--4.h)

normaliza :: Polinomio -> Polinomio

normaliza [] = []
normaliza [(c,e)] = [(c,e)]
normaliza ((c,e):(c2,e2):xs)
    |e == e2 = ((c + c2), e) : normaliza xs
    |otherwise = (c,e) : normaliza ((c2,e2):xs)   

--4.i)

soma :: Polinomio -> Polinomio -> Polinomio

soma p1 p2 = normaliza (p1 ++ p2)

--4.j)

produto :: Polinomio -> Polinomio -> Polinomio

produto [] _ = []
produto _ [] = []
produto ((c1,e2):xs1) p = normaliza((mult (c1,e2) p) ++ produto xs1 p)

--4.k)

ordena :: Polinomio -> Polinomio

ordena [] = []
ordena ((c,e):xs) = ordena [ (c2,e2) | (c2,e2) <- xs, e2 <= e]
                    ++ [(c,e)]
                    ++ ordena [ (c2,e2) | (c2,e2) <- xs, e2 > e]

--4.l)

equiv :: Polinomio -> Polinomio -> Bool

equiv p1 p2 = normaliza (ordena p1) == normaliza(ordena p2)

--FIM