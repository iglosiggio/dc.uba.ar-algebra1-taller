eAprox :: Integer -> Float
eAprox 0 = 1
eAprox n = eAprox' n 0
eAprox' 0 acc = acc + 1
eAprox' n acc = eAprox' (n - 1) (acc + 1 / fromInteger (fact n))

fact 0 = 1
fact n = fact' n 1
fact' 0 acc = acc
fact' n acc = fact' (n - 1) (acc * n)
