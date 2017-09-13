eAprox :: Integer -> Float
eAprox 0 = 1
eAprox n = eAprox' n 0
eAprox' 0 acc = acc + 1
eAprox' n acc = eAprox' (n - 1) (acc + 1 / fromInteger (fact n))

e = eAprox 100

fact 0 = 1
fact n = fact' n 1
fact' 0 acc = acc
fact' n acc = fact' (n - 1) (acc * n)

-- Si n pertenece al rango [a; b)
enRango n a b = n >= a && n < b

parteEntera n = parteEntera' n 0
parteEntera' n acc | enRango n 0 1 = acc
                   | n > 1 = parteEntera' (n - 1) (acc + 1)
                   | n < 0 = parteEntera' (n + 1) (acc - 1)

division a d = division' a d (0, 0)
division' a d (cociente, 0) | a < d = (cociente, a)
                            | otherwise = division' (a - d) d (cociente + 1, 0)
