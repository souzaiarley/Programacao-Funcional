-- IDENTIFICAÇÃO
matricula = "535779" -- coloque a matricula aqui entre as asspas

-- Nome
nome = "Iarley Natã Lopes Souza" -- coloque seu nome aqui entre aspas

-- ATIVIDADE 2

-- Esta atividade visa construir uma 
-- função que determine os n primeiros números primos

-- Construa as funções a seguir,

-- determina os divisores de x excluindo o 1
divisores :: Int -> [Int]
divisores x = [n | n <- [2..x], mod x n == 0] -- mude aqui

-- Determina se um números x é ou não primo
eprimo :: Int -> Bool
eprimo x = length ([n | n <- [2..x], mod x n == 0]) == 1 -- mude aqui

-- cria lista com n primeiros primos
primos :: Int -> [Int]
primos n = take n [x | x <- [2..], eprimo x]  -- mude aqui
