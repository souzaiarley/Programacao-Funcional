-- MÓDULOS

import System.IO
import System.Environment
-- não import mais nada!

--IDENTIFICAÇÃO

atividade = 8
matricula = "535779"
nome      = "Iarley Natã Lopes Souza"

-- ATIVIDADE

-- Construir programa que leia
-- na linha de comando três strings.
-- A primeira é um nome de arquivo válido, 
-- digamos, f. As outras são palavras,
-- w1 e w2. O programa deve susturuir
-- todas as aparições de w1 em f
-- por w2. Um arquivo de saída com as
-- modificações deve ser
-- a saída. Seu nome precisa se o de f
-- o arquivo de saída deter nome igual
-- com prefixo "subst-". .

-- MATENHA O .hs COM NOME
-- "atividade.hs" E CONSEQUENTEMENTE
-- EXECUTÁVEL COMO SENDO
-- "atividade08".

-- CÓDIGO


main = do
     args <- getArgs -- retorna argumentos
     contents <- readFile (head args)      -- na forma de uma lista
     let w1 = args !! 1
     let w2 = args !! 2    
     
     let newname = "subst-" ++ head args

     let wordsList = words contents
     
     writeFile newname (replace contents [x| x <- wordsList, compara x w1] w1 w2)

     return ()    

replace :: [Char] -> [[Char]] -> [Char] -> [Char] -> [Char]
replace [] _ _ _ = []
replace lista [] _ _ = lista
replace (x:xs) (y:ys) w1 w2
     | x == ' ' = x : replace xs (y:ys) w1 w2
     | compara (fst tuple) w1 = w2 ++ drop (length w1) y ++ replace (snd tuple) ys w1 w2
     | otherwise = x : replace xs (y:ys) w1 w2
     where tuple = span (/= ' ') (x:xs)
     
compara :: [Char] -> [Char] -> Bool
compara [] [] = True
compara [] sub = False
compara str [] = True
compara (x:xs) (y:ys)
     | x == y = compara xs ys
     | otherwise = False


-- INFORMAÇÕES

-- Compilação e execução

-- $ ghci atividade-08.hs
-- $ ./atividade-08 historia.txt Pedro Pablo

-- Onde "historia.txt" é um arquivo de texto
-- em que toda palavra "Pedro" é substituída
-- por "Pablo".

-- Exemplo

-- "historia.txt" de entrada,

-- Pedro vivia numa casa de pedra.
-- Mas Pdro queria morar numa 
-- casa de ouro. Pobre Pedro!

-- "subst-historia.txt" criado,

-- Peblo vivia numa casa de pedra.
-- Mas Pabloqueria morar numa 
-- casa de ouro. Pobre Pablo!
--