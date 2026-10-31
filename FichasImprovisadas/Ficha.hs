module FichaImprovisada where
import Data.Char
--1) Contar pares e ímpares

contaParImpar :: [Int] -> (Int, Int)

contaParImpar [] = (0,0)
contaParImpar (x:xs)
    |even x = (p + 1, i)
    |otherwise = (p, i + 1)
    where
        (p, i) = contaParImpar xs
        
--2) Contar negativos, zeros e positivos

conta :: [Int] -> (Int, Int, Int)

conta [] = (0, 0, 0)
conta (x:xs)
    |x < 0 = (n + 1, z, p)
    |x > 0 = (n, z, p + 1)
    |otherwise = (n, z + 1, p)
    where
        (n, z, p) = conta xs

--3) Separar letras e números

separa :: String -> (String, String)

separa [] = ([], [])

separa (x:xs)
    |isAlpha x = ((x : letras), digitos)
    |isDigit x = (letras, (x : digitos))
    |otherwise = separa xs
    where
        (letras, digitos) = separa xs

--4) Estatisticas de uma lista, a funcao devolve a soma dos pares, dos impares, e o numero de zeros

estatisticas [] = (0, 0, 0)
estatisticas (x:xs)
    | even x = (pares + x, impares, zeros) 
    | odd x = (pares, impares + x, zeros) 
    | otherwise = (pares, impares, zeros + 1) 
    where
        (pares, impares, zeros) = estatisticas xs