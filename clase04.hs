-- fibonacci (recursivo)
fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

-- fibbonacci (tail-call)
--ifib 1 = (1, 1)
--ifib n = let (curr, prev) = ifib (n - 1)
--          in (curr + prev, curr)

-- fibonacci (tail-call)
-- usage: ifib (1, 1) n
ifib (curr, prev) left | left <= 1 = curr
ifib (curr, prev) left | otherwise = ifib (curr + prev, curr) (left - 1)

-- sucesiones del ej 16 y 20 de la práctica 2
s1 1 = 2
s1 n = 2 * (n - 1) * s1 (n - 1) + 2^n * factorial (n - 1)

s2 1 = 1
s2 2 = 2
s2 n = (n - 2) * s2 (n - 1) + 2 * ((n - 2) + 1) * s2 (n - 2)

s3 1 = -3
s3 2 = 6 
s3 n | (mod n 2) == 0 = - s3 (n - 1) - 3
     | otherwise = s3 (n - 1) + 2 * s3 (n - 2) + 9

-- sumatoria desde 1 hasta n de i
-- idéntica a n * (n + 1) / 2
sumatoria 1 = 1
sumatoria n = n + sumatoria (n - 1)

-- sumatorias del ej 5 de la práctica 2
f1 0 = 2^0
f1 n = 2^n + f1 (n - 1)

f2 1 q = q^1
f2 n q = q^n + f2 (n - 1) q

-- detalle: es hasta 2n
-- f3 1 q = q^(2 * 1) + q^(2 * 1 - 1)
-- f3 n q = q^(2 * n) + q^(2 * n - 1) + f3 (n - 1) q
f3 n q = f2 (2 * n) q

f4 n q = ((f3 n q) - (f2 (n - 1) q))/2

esPar :: Integer -> Bool
esPar 0 = True
esPar 1 = False
esPar n = esPar (n - 2)

multiplo3 :: Integer -> Bool
multiplo3 0 = True
multiplo3 1 = False
multiplo3 2 = False
multiplo3 n = multiplo3 (n - 3)

multiplo n 0 = True
multiplo n m | m < 0 = multiplo n (abs m)
             | m >= n = multiplo n (m - n)
             | otherwise = False

sumaImpares n = psum 1 n (\i -> 2 * i - 1)

doblefact 0 = 1
doblefact n = pprod 1 (n/2) (\i -> 2 * i)

-- sumas parciales (recursivo)
psum from to expr | from == to = expr from
                  | otherwise  = expr from + psum (from + 1) to expr

pf1 n   = psum 0 n (\i -> 2^i)
pf2 n q = psum 1 n (\i -> q^i)
pf3 n q = psum 1 (2 * n) (\i -> q^i)
pf4 n q = psum n (2 * n) (\i -> q^i/2)

-- productos parciales (recursivo)
pprod from to expr | from == to = expr from
                   | otherwise  = expr from * pprod (from + 1) to expr

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = pprod 1 n id

-- sumas parciales (tail-call)
-- ipsum = ...
