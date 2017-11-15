fold :: (a -> b -> b) -> b -> [a] -> b
fold f acc [] = acc
fold f acc (x:xs) = f x (fold f acc xs)

tamañoLista :: [a] -> Integer
tamañoLista = (fold (\_ -> (+1)) 0)

sumar :: Num a => [a] -> a
sumar = (fold (+) 0)

transformar :: (a -> b) -> [a] -> [b]
transformar f = (fold transformarElemento [])
  where transformarElemento v lista = f v : lista

filtrar :: (a -> Bool) -> [a] -> [a]
filtrar f = (fold chequear [])
  where chequear v acc | f v       = v : acc
                       | otherwise = acc

--criba :: Integer -> [Integer]
--criba 1 = []
--criba n = [p | p <= [1..n], ]

--factorizar :: Integer -> [(Integer, Integer)]
--factorizar n = [(p, valuacion p n) | p <- criba n, mod n p == 0]

--valuacion :: Integer -> Integer -> Integer
--valuacion p n | mod n p == 0 = 1 + valuacion p (div n p)
--              | otherwise    = 0
