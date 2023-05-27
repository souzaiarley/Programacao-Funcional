-- IDENTIFICAÇÃO

atividade = 7

nome = "Iarley Natã Lopes Souza"

matricula = "535779"

-- TIPO DE DADOS

-- Representa polinômio como 
-- um vetor de seus coeficientes

-- através de seus coeficientes

data Poly = Poly [Float]

-- IMPLEMENTAR

-- Instância de Show que permite 
-- imprimir um polinômio

instance Show Poly where
    show (Poly lista) = printPoly (Poly lista) 0


printPoly :: Poly -> Int -> [Char]
printPoly (Poly []) _ = []
printPoly (Poly (0.0:as)) pot = printPoly (Poly as) (pot+1)
printPoly (Poly (a:as)) pot
    | pot == 0 = (show a) ++ printPoly (Poly as) (pot+1)
    | pot == 1 && signum a == 1 = "+" ++ show a ++ "x" ++ printPoly (Poly as) (pot+1)
    | pot == 1 && signum a == -1 = show a ++ "x" ++ printPoly (Poly as) (pot+1)
    | pot > 1 && signum a == 1 = "+" ++ show a ++ "X^" ++ show pot ++ printPoly (Poly as) (pot+1)
    | pot > 1 && signum a == -1 = show a ++ "X^" ++ show pot ++ printPoly (Poly as) (pot+1)
    | otherwise = []

-- Exemplos
-- Main> Poly [1,2,3]
-- 1.0+2.0x+3.0X^2
-- *Main> Poly [-2,1,0]
-- -2.0+1.0x
-- *Main> Poly [-1,0,-1]
-- -1.0-1.0X^2


aux_avalPoly :: Poly -> Float -> Int -> Float
aux_avalPoly (Poly []) _ _ = 0.0
aux_avalPoly (Poly (a:as)) x pot = a*(x^pot) + aux_avalPoly (Poly(as)) x (pot+1)


--AVALIAÇÃO DE POLINÔMIOS

-- Avalia um poliômio P 
-- dado x, ou seja, calcula P(x) 

avalPoly :: Poly -> Float -> Float
avalPoly (Poly lista) x = aux_avalPoly (Poly lista) x 0

-- Exemplos
-- *Main> avalPoly (Poly [1,2,3]) 5
-- 86.0
-- *Main> avalPoly (Poly [-1,1,3]) 5
-- 79.0
-- *Main> avalPoly (Poly [11,0,2,2]) 3
-- 83.0