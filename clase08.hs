type Set a = [a]

vacio :: Set Integer
vacio = []

existe :: Integer -> Set Integer -> Bool
existe = elem

agregar :: Integer -> Set Integer -> Set Integer
agregar n set | existe n set = set
              | otherwise    = n : set

incluido :: Set Integer -> Set Integer -> Bool
incluido [] _ = True
incluido (elemento:conjunto) set = existe elemento set
                                && incluido conjunto set

iguales :: Set Integer -> Set Integer -> Bool
iguales a b = incluido a b && incluido b a

agregarATodas :: Integer -> Set (Set Integer) -> Set (Set Integer)
agregarATodas n [] = []
agregarATodas n (lista:listas) = (agregar n lista) : (agregarATodas n listas)

partes :: Integer -> Set (Set Integer)
partes 0 = [[]]
partes n = agregarATodas n (partes (n - 1)) ++ partes (n - 1)

productoCartesiano :: Set Integer -> Set Integer -> Set (Integer, Integer)
productoCartesiano _ [] = []
productoCartesiano [] _ = []
productoCartesiano (x:xs) (y:ys) = (x, y) : productoCartesiano [x] ys
                                         ++ productoCartesiano xs  [y]
                                         ++ productoCartesiano xs  ys

-- Agregar aunque exista
sumarATodas :: Integer -> [[Integer]] -> [[Integer]]
sumarATodas n [] = []
sumarATodas n (lista:listas) = (n:lista) : (sumarATodas n listas)

insercionesPosibles :: Integer -> [Integer] -> [[Integer]]
insercionesPosibles n [] = [[n]]
insercionesPosibles n (x:xs) = [n : (x:xs)] ++ sumarATodas x (insercionesPosibles n xs)

insercionesPosiblesEnListas :: Integer -> [[Integer]] -> Set [Integer]
insercionesPosiblesEnListas n [] = []
insercionesPosiblesEnListas n (lista:listas) = insercionesPosibles n lista
                                            ++ insercionesPosiblesEnListas n listas

elementosUnicos :: [[Integer]] -> Set [Integer]
elementosUnicos [] = []
elementosUnicos (x:xs) | elem x xs = elementosUnicos xs
                       | otherwise = x : elementosUnicos xs

-- Chequear contra la referencia:
--   https://gist.github.com/finiteautomata/db7210851a8a2a76237fdfa914a06afa

variaciones :: Set Integer -> Integer -> Set [Integer]
variaciones _ 0  = [[]]
variaciones [] _ = []
variaciones (x:xs) 1 = [x] : variaciones xs 1
variaciones (x:xs) n = elementosUnicos 
                         (insercionesPosiblesEnListas x (variaciones (x:xs) (n - 1))
                          ++ variaciones xs n)

