module Aritmetica where
import Catedra
import Data.Tuple
import Data.Bits


--(1)
mcdExt :: Integer -> Integer -> (Integer, (Integer, Integer))
mcdExt _ _ = (0, (0, 0))

--(2)
criba :: Integer -> Set Integer
criba n = cribaDesde 2 n []

cribaDesde :: Integer -> Integer -> Set Integer -> Set Integer
cribaDesde desde hasta primos | desde == hasta = primos
    | desde < hasta && loDivideAlguno desde primos = cribaDesde (desde + 1) hasta primos
    | desde < hasta = cribaDesde (desde + 1) hasta (desde:primos)

loDivideAlguno :: Integer -> Set Integer -> Bool
loDivideAlguno _ [] = False
loDivideAlguno n (x:xs) = esDivisor x n || loDivideAlguno n xs

filtrarDivisores :: Integer -> Set Integer -> Bool
filtrarDivisores n [] = []
filtrarDivisores n (x:xs) | esDivisor x n = filtrarDivisores n xs
                          | otherwise = x : filtrarDivisores n xs

esDivisor :: Integer -> Integer -> Bool
esDivisor d n = mod n d == 0

--(3)
coprimoCon:: Integer -> Integer
coprimoCon n = buscarCoprimo n filtrarDivisores n (criba n)


--(4)
inversoMultiplicativo:: Integer -> Integer -> Integer
inversoMultiplicativo _ _ = 0



-- Función de regalo para exponenciar "rápido"
modExp :: Integer -> Integer -> Integer -> Integer
modExp b 0 m = 1
modExp b e m = t * modExp ((b * b) `mod` m) (shiftR e 1) m `mod` m
  where t = if testBit e 0 then b `mod` m else 1
