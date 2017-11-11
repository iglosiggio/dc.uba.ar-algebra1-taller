module Crack where
import Catedra
import Aritmetica
import RSA


--(9)
romper :: Clpub -> Clpri
romper (e,n) = (inversoMultiplicativo e phi, n)
  where (p,q) = factorizarN n
        phi = (p - 1) * (q - 1)

factorizarN :: Integer -> (Integer, Integer)
factorizarN n = (divisor, div n divisor)
  where divisor = hallarUnDivisor n

-- No definida si n es primo
hallarUnDivisor :: Integer -> Integer
hallarUnDivisor n = divisor
  where (divisor:_) = (sóloDivisores n primosCandidato)
        primosCandidato = criba (raizEnteraAprox n)

raizEnteraAprox :: Integer -> Integer
raizEnteraAprox n = fromIntegral (truncate (sqrt (fromInteger n)))

--(8)
espia :: Clpub -> Cifrado -> Mensaje
espia publica msj = decodificador (romper publica) msj
