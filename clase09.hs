type Polinomio = [Float]

evaluar :: Polinomio -> Float -> Float
evaluar [] _ = 0
evaluar (coeff:coeffs) x = coeff * x ^ (length coeffs) + (evaluar coeffs x)

derivada :: Polinomio -> Polinomio
derivada [] = []
derivada [k] = []
derivada (coeff:coeffs) = coeff * fromIntegral (length coeffs) : derivada coeffs

derivadaNesima :: Integer -> Polinomio -> Polinomio
derivadaNesima 0 polinomio = polinomio
derivadaNesima n polinomio = derivadaNesima (n-1) (derivada polinomio)

quitarCeros :: Polinomio -> Polinomio
quitarCeros (0:xs) = quitarCeros xs
quitarCeros polinomio = polinomio

sumarCon0 :: Polinomio -> Polinomio -> Polinomio
sumarCon0 [] polinomio = polinomio
sumarCon0 polinomio [] = polinomio
sumarCon0 (x:xs) (y:ys) | length xs < length ys  = y : sumar (x:xs) ys
                        | length xs == length ys = x + y : sumar xs ys
                        | length xs > length ys  = x : sumar xs (y:ys)

sumar :: Polinomio -> Polinomio -> Polinomio
sumar a b = quitarCeros (sumarCon0 a b)

productoPorEscalar :: Float -> Polinomio -> Polinomio
productoPorEscalar 0 _ = []
productoPorEscalar _ [] = []
productoPorEscalar x (coeff:coeffs) = x * coeff : productoPorEscalar x coeffs
