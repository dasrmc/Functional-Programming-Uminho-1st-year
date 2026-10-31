module Ficha4 where
import Data.Char

--1)

digitAlpha :: String -> (String, String)

digitAlpha string =
    let
        letras = [l | l <- string, isAlpha l]
        digitos = [l | l <- string, isDigit l]
    in
        (letras, digitos)

nzp :: [Int] -> (Int, Int, Int)

nzp [] = (0, 0, 0)
nzp (x:xs)
    |x < 0 = (n + 1, z, p)
    |x == 0 = (n, z + 1, p)
    |otherwise = (n, z, p + 1)
    where
        (n, z, p) = nzp xs
    