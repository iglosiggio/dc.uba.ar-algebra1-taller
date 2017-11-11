module Aritmetica where
import Catedra
import Data.Tuple
import Data.Bits


--(1)
-- robado del ejercicio de clase10.hs
mcdExt :: Integer -> Integer -> (Integer, (Integer, Integer))
mcdExt g 0 = (g, (1, 0))
mcdExt a b = (g, (s, t))
  where q = div a b
        (g, (s1, t1)) = mcdExt b (a - q * b)
        s = t1
        t = s1 - t1 * q

--(2)
criba :: Integer -> Set Integer
criba n = cribaDesde 2 n []

cribaDesde :: Integer -> Integer -> Set Integer -> Set Integer
cribaDesde desde hasta primos | desde == hasta = primos
                              | desde < hasta
                                && loDivideAlguno desde primos = cribaDesde (desde + 1) hasta primos
                              | desde < hasta = cribaDesde (desde + 1) hasta (desde:primos)

loDivideAlguno :: Integer -> Set Integer -> Bool
loDivideAlguno _ [] = False
loDivideAlguno n (x:xs) = esDivisor x n || loDivideAlguno n xs

sóloDivisores :: Integer -> Set Integer -> Set Integer
sóloDivisores n [] = []
sóloDivisores n (x:xs) | esDivisor x n = x : sóloDivisores n xs
                       | otherwise     = sóloDivisores n xs

esDivisor :: Integer -> Integer -> Bool
esDivisor d n = mod n d == 0

--(3)
-- busco desde n / 3 hasta n - 1
coprimoCon :: Integer -> Integer
coprimoCon n = buscarCoprimo n (div n 3) (n - 1)

-- La función no está definida si no hay coprimos en el intervalo [desde; hasta]
buscarCoprimo :: Integer -> Integer -> Integer -> Integer
buscarCoprimo n candidato hasta | candidato < hasta 
                                  && sonCoprimos candidato n = candidato
                                | candidato < hasta = buscarCoprimo n (candidato + 1) hasta

sonCoprimos :: Integer -> Integer -> Bool
sonCoprimos a b = mcd == 1
  where (mcd, _) = mcdExt a b

--(4)
-- Sólo es válido si n y p son coprimos
-- fuente: https://es.wikipedia.org/wiki/Inverso_multiplicativo_(aritm%C3%A9tica_modular)
inversoMultiplicativo:: Integer -> Integer -> Integer
inversoMultiplicativo a n | mcd == 1 && inverso > 0 = inverso
                          | mcd == 1 = n + inverso
  where (mcd, (inverso, _)) = mcdExt a n

-- Función de regalo para exponenciar "rápido"
modExp :: Integer -> Integer -> Integer -> Integer
modExp b 0 m = 1
modExp b e m = t * modExp ((b * b) `mod` m) (shiftR e 1) m `mod` m
  where t = if testBit e 0 then b `mod` m else 1
