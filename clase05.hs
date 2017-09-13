eAprox :: Integer -> Float
eAprox 0 = 1
eAprox x = eAprox (x - 1) + 1 / fact (fromInteger x)

fact 0 = 1
fact n = n * fact (n - 1)

fact' 0 acc = acc
fact' n acc = fact' (n - 1) (acc * n)
