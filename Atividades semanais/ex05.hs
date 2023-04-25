-- ATIVIDADE 
atividade = 5

-- IDENTIFICAÇÃO
matricula = "535779" -- coloque a matricula aqui entre as asspas

-- Nome
nome = "Iarley Natã Lopes Souza" -- coloque seu nome aqui entre aspas

-- 1

-- FUNÇÕES HASKELL A FAZER,

-- Construa função que 
-- receba uma string e 
-- retorne a lista das 
-- tuplas das frequencias dos
-- seus caracteres
freq :: [Char] -> [(Char, Int)]
freq [] = []
freq s = (head s, length [x | x <- s, x == head s]) : freq [x | x <- s, x /= head s]

-- Exemplos:

-- >> freq "abcdaadd"
-- [('a',3), ('b',1),('c',1),('d',3)]
-- >> freq "A casa"
-- [('A',1), ('a', 2), ('c',1), ('s', 1), (' ',1) ]

-- Se existir uma função em
-- Haskell que faça a mesma coisa, não use.

-- 2

-- Construa função que ordene
-- a lista de tuplas da questão
-- por valor de frequencia,

freqSort :: [(Char, Int)] -> [(Char, Int)]
freqSort [] = []
freqSort list = freqSort [x | x <- list, (snd x) <= snd (head list), (fst x) /= fst (head list)] ++ [head list] ++ freqSort [x | x <- list, (snd x) > snd (head list)] 

-- Exemplos,

-- >> s = freqSort freq "aaaa22p"
-- [('p',1), ('2', 2), ('a', 4)]

-- Obs: Se existir uma função 
-- em Haskell que faça a mesma coisa, 
-- não use.