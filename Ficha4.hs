module Ficha4 where
import Data.Char

--1)

--digitAlpha :: String -> (String, String)

--digitAlpha string =
--    let
--        letras = [l | l <- string, isAlpha l]
--        digitos = [l | l <- string, isDigit l]
--    in
--        (letras, digitos)

nzp :: [Int] -> (Int, Int, Int)

nzp [x, y, z] =
    let
        x2 = [x | x <- [int], x < 0]
        y2 = [y | y <- [int], x == 0]
        z2 = [z | z <- [int], x > 0]
    in
        (x2, y2, z2)