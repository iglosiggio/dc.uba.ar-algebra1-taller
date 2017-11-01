mcd :: Integer -> Integer -> Integer
mcd rta 0 = rta
mcd a b = mcd b (a - (div a b) * b)

emcd :: Integer -> Integer -> (Integer, Integer, Integer)
emcd g 0 = (g, 1, 0)
emcd a b = (g, s, t)
         where q = div a b
               (g, s1, t1) = emcd b (a - q * b)
               s = t1
               t = s1 - t1 * q

verificar_emcd :: Integer -> Integer -> Bool
verificar_emcd a b = gcd a b == s*a + t*b
                   where (_, s, t) = emcd a b

verificar_mcd :: Integer -> Integer -> Bool
verificar_mcd a b = gcd a b == mcd a b

tieneSolucion :: Integer -> Integer -> Integer -> Bool
tieneSolucion a b m = mod b (mcd a m) == 0

-- Esto esta mal hecho
--solucionDiofantica :: Integer -> Integer -> Integer -> (Integer, Integer)
--solucionDiofantica a b m | tieneSolucion a b m = (m' * s, m' * t)
--                         where q = mcd a m
--                               a' = div a q
--                               b' = div b q
--                               m' = div m q
--                               (g, s, t) = emcd a' b'
