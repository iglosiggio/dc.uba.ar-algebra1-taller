module Aritmetica where
import Catedra
import Data.Tuple
import Data.Bits


--(1)
mcdExt :: Integer -> Integer -> (Integer, (Integer, Integer))
mcdExt _ _ = (0, (0, 0))

--(2)
criba :: Integer -> Set Integer
criba _ = []

--(3)
coprimoCon:: Integer -> Integer
coprimoCon _ = 0


--(4)
inversoMultiplicativo:: Integer -> Integer -> Integer
inversoMultiplicativo _ _ = 0



-- Función de regalo para exponenciar "rápido"
modExp :: Integer -> Integer -> Integer -> Integer
modExp b 0 m = 1
modExp b e m = t * modExp ((b * b) `mod` m) (shiftR e 1) m `mod` m
  where t = if testBit e 0 then b `mod` m else 1
