y_logico :: Bool -> Bool -> Bool
y_logico True True = True
y_logico _    _    = False

o_logico :: Bool -> Bool -> Bool
o_logico True _    = True
o_logico _    True = True
o_logico _    _    = False

xor_logico :: Bool -> Bool -> Bool
xor_logico True  False = True
xor_logico False True  = True
xor_logico _     _     = False

implica_logico :: Bool -> Bool -> Bool
implica_logico True  False = False
implica_logico _     _     = True

suma_gauss :: Integer -> Integer
suma_gauss 0 = 0
suma_gauss n = n + suma_gauss(n - 1)

algun_0 :: (Integer, Integer, Integer) -> Bool
algun_0 (0, _, _) = True
algun_0 (_, 0, _) = True
algun_0 (_, _, 0) = True
algun_0 (_, _, _) = False

prod_interno :: (Float, Float) -> (Float, Float) -> Float
prod_interno (x, y) (i, j) = x * i + y * j

es_par n = (mod n 2) == 0

-- collatz length until reaches 1
lcollatz :: Integer -> Integer
lcollatz 1 = 1
lcollatz n | es_par n  = 1 + lcollatz (div n 2)
           | otherwise = 1 + lcollatz (3 * n + 1)

mayor_lcollatz 0 mayor = mayor
mayor_lcollatz n (m, mlength) | lcollatz n > mlength = mayor_lcollatz (n - 1) (n, lcollatz n)
                              | otherwise            = mayor_lcollatz (n - 1) (m, mlength)

mayor_lcollatz_hasta_10000 = mayor_lcollatz 10000 (0, 0)

-- n tiene divisores hasta m (desde 2)
tiene_divisores_hasta 2 n = not (es_par n)
tiene_divisores_hasta m n | (mod n m) == 0 = False
                          | otherwise      = tiene_divisores_hasta (m - 1) n
es_primo 0 = False
es_primo 1 = False
es_primo 2 = True
es_primo n = tiene_divisores_hasta (n - 1) n

tiene_suma_de_primos_hasta 0 n = False
tiene_suma_de_primos_hasta a n | (es_primo a) && (es_primo (n - a)) = True
                               | otherwise = tiene_suma_de_primos_hasta (a - 1) n
es_suma_de_primos n = tiene_suma_de_primos_hasta n n


vale_goldbach_hasta 0 = True
vale_goldbach_hasta 1 = True
vale_goldbach_hasta 2 = True
vale_goldbach_hasta n | es_par n  = (es_suma_de_primos n) && vale_goldbach_hasta (n - 2)
                      | otherwise = vale_goldbach_hasta (n - 1)

suma_digitos 0 = 0
suma_digitos n = (mod n 10) + suma_digitos(div n 10)

todos_digitos_iguales_a digito 0 = digito == 0
todos_digitos_iguales_a digito n
  | n == digito           = True
  | otherwise = (digito == (mod n 10)) && todos_digitos_iguales_a digito (div n 10)

todos_digitos_iguales n = todos_digitos_iguales_a (mod n 10) (div n 10)
