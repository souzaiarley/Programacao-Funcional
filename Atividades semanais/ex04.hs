-- ATIVIDADE 
atividade = 4

-- IDENTIFICAÇÃO
matricula = "535779" -- coloque a matricula aqui entre as asspas

-- Nome
nome = "Iarley Natã Lopes Souza" -- coloque seu nome aqui entre aspas

-- 1

-- FUNÇÕES HASKELL A FAZER,

-- Implementar função que receba uma lista
-- ou string de entrada e retorne uma outra 
-- equivalente sem repetiições de elementos,

unique :: Eq a => [a] -> [a]
unique [] = []
unique s = head s : [n | n <- unique (tail s), n /= head s] -- implemente aqui


-- Exemplos:

-- >> unique "a1abaa1123b"
-- "a1b23"
-- >> unique [2,1,1,3,3,1,1,3,2]
-- [2,1,3]

-- Obs: (1) Note que a ordem relativa das chaves
-- remanescentes se preserva. (2) Se existir uma função em
-- Haskell que faça a mesma coisa, não deve ser usada. 


-- 2

-- Construa função que remova o valor mínimo de uma lista. 

delete'min :: (Ord a) => [a] -> [a]
delete'min [] = []
delete'min x   -- implemente aqui
    | head x == minimum x = tail x
    | otherwise = head x : delete'min (tail x)


-- Exemplos,

-- >> delete'min [1,3,2,5]
-- [3,2,5]
-- >> delete'min [7,3,2,5,6]
-- [7,3, 5,6]

-- Obs: (1) Se o valor mínimo se repetir
-- então somente a primeira aparição deve 
-- ser removida. (2) Se existir uma função 
-- em Haskell que faça a mesma coisa, 
-- não deve se utilizada