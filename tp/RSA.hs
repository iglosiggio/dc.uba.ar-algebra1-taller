module RSA where
import Catedra
import Aritmetica


--(5)
claves :: Integer -> Integer -> (Integer, Integer, Integer)
claves p q = (e, d, n)
  where n = p * q
        e = coprimoCon phi
        d = inversoMultiplicativo e phi
        phi = (p - 1) * (q - 1)

--(6)
codificador :: Clpub -> Mensaje -> Cifrado
codificador clave msj = cifrar clave (aEnteros msj)

cifrar :: Clpub -> [Integer] -> Cifrado
cifrar _ [] = []
cifrar (e,n) (c:cs) | sonCoprimos c n = modExp c e n : cifrar (e,n) cs
                    | otherwise       = (-c) : cifrar (e,n) cs

--(7)
decodificador :: Clpri -> Cifrado -> Mensaje
decodificador clave msj = aChars (decifrar clave msj)

decifrar :: Clpri -> Cifrado -> [Integer]
decifrar _ [] =  []
decifrar (d,n) (c:cs) | c >= 0    = modExp c d n : decifrar (d,n) cs
                      | otherwise = (-c) : decifrar (d,n) cs
