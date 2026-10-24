module Ficha3 where

-- 3) Definição do tipo
data Hora = H Int Int deriving (Show,Eq)

type Etapa = (Hora,Hora)
type Viagem = [Etapa]

-- 3.a) testarHoras
testarHoras :: Hora -> Bool
testarHoras (H h m) =
    h >= 0 && h <= 23 &&
    m >= 0 && m <= 59

-- 3.b) compHoras
compHoras :: Hora -> Hora -> Bool
compHoras (H h m) (H h2 m2) =
    h > h2 || (h == h2 && m > m2)

-- 3.c) cvrtHoraParaM
cvrtHoraParaM :: Hora -> Int
cvrtHoraParaM (H h m) = h * 60 + m

-- 3.d) cvrtMinParaH
cvrtMinParaH :: Int -> Hora
cvrtMinParaH m = H (m `div` 60) (m `mod` 60)

-- 3.e) difHoras (versão correta)
difHoras :: Hora -> Hora -> Int
difHoras h1 h2 =
    abs (cvrtHoraParaM h1 - cvrtHoraParaM h2)

-- 3.f) adicionarMin
adicionarMin :: Hora -> Int -> Hora
adicionarMin h m1 = cvrtMinParaH (cvrtHoraParaM h + m1)


--Ficha 3)
--1.a)

testarEtapa :: Etapa -> Bool

testarEtapa (H h m,H h1 m1) = testarHoras (H h m)
                                && testarHoras (H h1 m1) 
                                && cvrtHoraParaM (H h1 m1) > cvrtHoraParaM (H h m)

--1.b)

testarViajem :: Viagem -> Bool

testarViajem [] = True
testarViajem [(H h1 m1, H h2 m2)] = testarEtapa (H h1 m1, H h2 m2)
testarViajem ((H h1 m1,H h2 m2):(H h3 m3, H h4 m4):xs) = testarEtapa (H h1 m1, H h2 m2)
                                        && testarEtapa (H h3 m3, H h4 m4)
                                        && cvrtHoraParaM (H h2 m2) < cvrtHoraParaM (H h3 m3)
                                        && testarViajem ((H h3 m3, H h4 m4):xs)

--1.c)

calcularHoraPC :: Viagem -> Etapa

calcularHoraPC l = (fst (head l), snd(last l))

--1.d)

calcularTempoTotal :: Viagem -> Int

calcularTempoTotal [] = 0
calcularTempoTotal ((H h m, H h2 m2):xs) = cvrtHoraParaM (H h2 m2) - cvrtHoraParaM (H h m) + calcularTempoTotal xs 

--1.e)

calcularTempoEspera :: Viagem -> Int

calcularTempoEspera [] = 0
calcularTempoEspera [(p, c)] = 0
calcularTempoEspera ((p1, c1):(p2, c2):xs) = cvrtHoraParaM p2 - cvrtHoraParaM c1 + calcularTempoEspera ((p2, c2):xs)

--1.f)

calcularTotal :: Viagem -> Int

calcularTotal l = calcularTempoTotal l + calcularTempoEspera l

--2)





data Ponto = Cartesiano Double Double | Polar Double Double deriving (Show,Eq)
type Poligonal = [Ponto]
data Figura = Circulo Ponto Double | Rectangulo Ponto Ponto | Triangulo Ponto Ponto Ponto deriving (Show,Eq)

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

--2.a)

comprimentoPoligonal :: Poligonal -> Double

comprimentoPoligonal [] = 0
comprimentoPoligonal [_] = 0
comprimentoPoligonal (p1:p2:xs) = dist2 p1 p2 + comprimentoPoligonal (p2:xs)

--2.b)

testarLinhaFechada :: Poligonal -> Bool

testarLinhaFechada l = head l == last l

--2.c)

triangula :: Poligonal -> [Figura]

triangula (p1:p2:p3:xs) = Triangulo p1 p2 p3 : triangula (p1:p3:xs)
triangula _ = []

--2.d)

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

areaPoligonal :: Poligonal -> Double
areaPoligonal p = sum (map area (triangula p))

--2.e)

--mover :: Poligonal -> Ponto -> Poligonal

--mover :: Poligonal -> Ponto -> Poligonal
--mover [] q = [q]
--mover (_:xs) q = q : xs

--2.f)

--zoom :: Double -> Poligonal -> Poligonal
--PERGUNTAR AO SOR

--3)

data Contacto = Casa Integer | Trab Integer | Tlm Integer | Email String deriving Show
type Nome = String
type Agenda = [(Nome, [Contacto])]

--3.a)

acrescEmail :: Nome -> String -> Agenda -> Agenda

acrescEmail _ _ [] = []
acrescEmail n e ((nome, contactos):xs)
    |n == nome = (nome, Email e : contactos) : xs
    |otherwise = (nome, contactos) : acrescEmail n e xs

--3.b)

verEmails :: Nome -> Agenda -> Maybe [String]

verEmails _ [] = Nothing
verEmails n ((nome, contactos):xs)
    |n == nome = Just [e | Email e <- contactos]
    |otherwise = verEmails n xs

--3.c)

consTelefs :: [Contacto] -> [Integer]

consTelefs [] = []
consTelefs contactos = [tlm | Tlm tlm <- contactos]
                        ++ [trab | Trab trab <- contactos]
                        ++ [casa | Casa casa <- contactos]

--3.d)

procuraCasa :: [Contacto] -> Maybe Integer
procuraCasa [] = Nothing
procuraCasa (Casa c:xs) = Just c
procuraCasa (_:xs) = procuraCasa xs

casa :: Nome -> Agenda -> Maybe Integer

casa _ [] = Nothing
casa n ((nome,contactos):xs)
    |n == nome = procuraCasa contactos
    |otherwise = casa n xs

--4)

type Dia = Int
type Mes = Int
type Ano = Int
type Nomee = String

data Data = D Dia Mes Ano deriving Show

type TabDN = [(Nomee,Data)]

--4.a)

procura :: Nomee -> TabDN -> Maybe Data

procura _ [] = Nothing

procura n ((nome, d):xs)
    |n == nome = Just d
    |otherwise = procura n xs

--4.b)

idade :: Data -> Nomee -> TabDN -> Maybe Int

idade _ _ [] = Nothing
idade dd n ((nome, d):xs)
    |n == nome = 
        let D da ma aa = dd
            D dn mn an = d
            idadeBase = aa - an
        in if (ma < mn) || (ma == mn && da < dn)
                then Just (idadeBase - 1)
                else Just idadeBase
    |otherwise = idade dd n xs --estudar mais este exercicio que e fdd pa crl

--4.c)

anterior :: Data -> Data -> Bool

anterior (D d1 m1 a1) (D d2 m2 a2) = a1 < a2 
                                || (a1==a2 && m1 < m2)
                                || (a1==a2 && m1 == m2 && d1 < d2)

--4.d)

ordena :: TabDN -> TabDN

ordena [] = []
ordena ((nomeP, dataP):xs) = 
    ordena [(n,d) | (n,d) <- xs, d `anterior` dataP]
    ++ [(nomeP, dataP)]
    ++ ordena [(n, d) | (n,d) <- xs, not (d `anterior` dataP)]

    --estudar quickSort

--4.e)

calcIdade :: Data -> Data -> Int
calcIdade (D da ma aa) (D dn mn an) =
    let idadeBase = aa - an
    in if (ma < mn) || (ma == mn && da < dn)
           then idadeBase - 1
           else idadeBase

porIdade :: Data -> TabDN -> [(Nome, Int)]

porIdade _ [] = []
porIdade da tabela =
    let tabelaOrd = ordena tabela
    in map (\(n,d) -> (n, calcIdade da d)) tabelaOrd 
    --estudar funçoes anonimas

--5)

data Movimento = Credito Float | Debito Float deriving Show
data Dataa = DD Int Int Int deriving Show
data Extracto = Ext Float [(Dataa, String, Movimento)] deriving Show

--5.a)

extValor :: Extracto -> Float -> [Movimento]

extValor (Ext saldo movs) x =
    let
        valor (Credito v) = v
        valor (Debito v)  = v
    in
        [m | (_,_,m) <- movs, valor m > x] --Estudar

--5.b)

--filtro :: Extracto -> [String] -> [(Data, Movimento)]

--filtro (Ext saldo movs) lista = [(d, m) | (d,s,m) <- movs, s `elem` lista ]

--5.c)

creDeb :: Extracto -> (Float, Float)

creDeb (Ext saldo movs) =
    let
        totalCred = [m | (_, _, Credito m) <- movs]
        totalDeb = [m | (_, _, Debito m) <- movs]
    in
        (sum totalCred, sum totalDeb)

--5.d)

saldo :: Extracto -> Float

saldo (Ext saldoInicial movs) =
    let
        totalCred = [m | (_, _, Credito m) <- movs]
        totalDeb = [m | (_, _, Debito m) <- movs]
    in saldoInicial + sum (totalCred) - sum(totalDeb)

--FIM