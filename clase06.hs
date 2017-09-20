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
