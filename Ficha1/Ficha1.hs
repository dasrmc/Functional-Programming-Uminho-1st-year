module Ficha1 where
import Data.Char

--a) Perimetro

perimetro :: Double -> Double
perimetro x = 2*pi*x

--b) Distância entre dois pontos no plano Cartesiano

dist :: (Double,Double) -> (Double,Double) -> Double
dist (a,b) (c,d) = sqrt((c-a)^2 + (d-b)^2)

--c) Recebe uma lista e devolve um par com o primeiro e último elemento dessa lista

primUlt :: [Int] -> (Int,Int)
primUlt l = (head l, last l)

--d) Recebe dois números e testa se o primeiro e mutiplo do segundo

multiplo :: Int -> Int -> Bool
multiplo m n = mod m n == 0

--e) Recebe uma lista e se o cumprimento da lista for impar retira-lhe o primeiro elemento, caso contrario devolve a lista

truncaImpar :: [Int] -> [Int]
truncaImpar l =
    if mod (length l) 2 == 1
    then tail l
    else l

--f) Calcula o maior de dois numeros inteiros

max2 :: Int -> Int -> Int
max2 x y =
    if x>y
    then x
    else y

--g) Calcula o maior de tres numeros inteiros, usando a funçao max2

max3 :: Int -> Int -> Int -> Int
max3 x y z =
    if (max2 x y) < z
    then z
    else (max2 x y)

--2.a) Recebe os (3) coeficientes de um polinomio de 2o grau e que calcula o numero de raızes (reais) desse polinomio

nRaizes :: Double -> Double -> Double -> Int
nRaizes a b c
    |b^2 - 4 * a * c < 0 = 0
    |b^2 - 4 * a * c == 0 = 1
    |otherwise = 2

--2.b) Usando a funçao anterior, recebe os coeficientes do polinomio e calcula a lista das suas raızes reais

raizes :: Double -> Double -> Double -> [Double]
raizes a b c
    |nRaizes a b c == 0 = []
    |nRaizes a b c == 1 = [(-b)/(2*a)] 
    |nRaizes a b c == 2 = [(-b - sqrt(b^2 - 4 * a * c)) / (2*a),(-b + sqrt(b^2 - 4 * a * c)) / (2*a)]

--3.a) 

type Hora = (Int, Int)
testarHoras :: Hora -> Bool
testarHoras (h,m) =
    h >= 0 && h <= 23 &&
    m >= 0 && m <= 59

--3.b)

compHoras :: Hora -> Hora -> Bool
compHoras (h,m) (h2,m2) =
    h > h2 || (h == h2 && m > m2)

--3.c)

cvrtHoraParaM :: Hora -> Int
cvrtHoraParaM (h,m) = h * 60 + m

--3.d)

cvrtMinParaH :: Int -> (Int,Int)
cvrtMinParaH m = (m `div` 60, m `mod` 60)

--3.e)

difHoras :: Hora -> Hora -> Int
difHoras (h1,m1) (h2,m2)
    |m1 >= m2 = m1 - m2
    |otherwise = m2 - m1
    where 
        cvrtHoraParaM (h1,m1) = m1
        cvrtHoraParaM (h2,m2) = m2

--3.f)

adicionarMin :: Hora -> Int -> Hora
adicionarMin (h, m) m1 = cvrtMinParaH (cvrtHoraParaM (h,m) + m1)

--4) Basicamente e repitir todos os exercicios mas substituir o sinonimo por H x y!

--5.a)

data Semaforo = Verde | Amarelo | Vermelho deriving (Show,Eq)

next :: Semaforo -> Semaforo
next Verde = Amarelo
next Amarelo = Vermelho 
next Vermelho = Verde

--5.b)

stop :: Semaforo -> Bool
stop Verde = False
stop Amarelo = False
stop Vermelho = True

--5.c)

safe :: Semaforo -> Semaforo -> Bool
safe s s2
    | s == Vermelho && s2 == Verde = True
    | s == Verde && s2 == Vermelho = True
    | otherwise = False

--6.a)

data Ponto = Cartesiano Double Double | Polar Double Double deriving (Show,Eq)

posx :: Ponto -> Double
posx (Cartesiano x y) = x
posx (Polar r a) = r * cos a

--6.b)

posy :: Ponto -> Double
posy (Cartesiano x y) = y
posy (Polar r a) = r * sin a

--6.c)

raio :: Ponto -> Double
raio (Cartesiano x y) = sqrt(x^2 + y^2)
raio (Polar r a) = r

--6.d)

angulo :: Ponto -> Double
angulo (Cartesiano x y) = atan2 y x -- igual a atan(y/x)
angulo (Polar r a) = a

--6.e)

dist2 :: Ponto -> Ponto -> Double
dist2 p1 p2 = sqrt((posx p2 - posx p1)^2 + (posy p2 - posy p1)^2)

--7.a)

data Figura = Circulo Ponto Double | Rectangulo Ponto Ponto | Triangulo Ponto Ponto Ponto deriving (Show,Eq)

poligono :: Figura -> Bool
poligono (Circulo _ _) = False
poligono _ = True --Estudar esta forma de fazer

--7.b)

vertices :: Figura -> [Ponto]
vertices (Circulo _ _) = []
vertices (Rectangulo a b) = [a, b]
vertices (Triangulo c d e) = [c, d, e]

--7.c)

area :: Figura -> Double
area (Triangulo p1 p2 p3) =
    let a = dist2 p1 p2
        b = dist2 p2 p3
        c = dist2 p3 p1
        s = (a+b+c) / 2                    -- semi-perimetro
    in sqrt (s*(s-a)*(s-b)*(s-c))          -- formula de Heron 
area (Rectangulo (Cartesiano x y) (Cartesiano x2 y2)) = 
    abs (x2-x) * abs (y2-y)
area (Circulo _ r) =
    pi * r^2

-- repetir estes exercicios todos do 7

--8.a)

isLower :: Char -> Bool
isLower a = ord a >= 97 && ord a <= 122

--8.b)

isDigit :: Char -> Bool
isDigit a = ord a >=48 && ord a <= 57

--8.c)

isAlpha :: Char -> Bool
isAlpha a = (ord a >= 65 && ord a <= 90) || (ord a >= 97 && ord a <= 122)

--8.d)

toUpper :: Char -> Char
toUpper a 
    | Ficha1.isLower a = chr(ord a - 32)
    | otherwise = a

--8.e)

intToDigit :: Int -> Char
intToDigit a = chr(a + 48)

--8.f)

digitToInt :: Char -> Int
digitToInt a = ord(a) - 48

--FIM



