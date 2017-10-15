enProgresion :: Integer -> Integer -> Integer -> Bool
enProgresion a b c | a > b = enProgresion b a c
                   | b > c = enProgresion a c b
                   | otherwise = (a - b) == (b - c)

valuacion2Adica :: Integer -> Integer
valuacion2Adica n | mod n 2 == 0 = 1 + valuacion2Adica (div n 2)
                  | otherwise = 0

sucesion :: Integer -> Integer
sucesion 1 = 3
sucesion n = 2 * sucesion (n - 1) + 3

_cuantosTerminos :: Integer -> Integer -> Integer
_cuantosTerminos n terminosValidos | sucesion (terminosValidos + 1) < n = _cuantosTerminos n (terminosValidos + 1)
                                   | otherwise = terminosValidos

cuantosTerminos :: Integer -> Integer
cuantosTerminos n = _cuantosTerminos n 0

esTipoFibonacci :: [Integer] -> Bool
esTipoFibonacci (anterior:actual:ultimo:[]) = anterior + actual == ultimo
esTipoFibonacci (anterior:actual:proximo:resto) = anterior + actual == proximo
                                               && esTipoFibonacci (actual:proximo:resto)

desplazar :: Integer -> [Integer] -> [Integer]
desplazar 0 lista = lista
desplazar n [] = []
desplazar n (x:xs) = desplazar (n - 1) (xs ++ [x])
