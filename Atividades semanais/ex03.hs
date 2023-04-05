-- IDENTIFICAÇÃO
matricula = "535779" -- coloque a matricula aqui entre as asspas

-- Nome
nome = "Iarley Natã Lopes Souza" -- coloque seu nome aqui entre aspas

-- ATIVIDADE 3

-- Remove espaços existentes no iníci
-- e final de uma strings dada.

strip :: [Char] -> [Char]
strip xs
    | xs == [] = []
    | head xs == ' ' && last xs == ' ' = strip (init (tail xs))
    | head xs == ' ' && last xs /= ' ' = strip (tail xs)
    | head xs /= ' ' && last xs == ' ' = strip (init xs)
    | otherwise = xs
    

-- Separa a primeira palavra do restante
-- de uma string (Palavras são substeings 
-- separadas por espaços). Exemplo,

-- >> popWord "casa  de tijolos"
-- ["casa", "de tijolos"]'
-- >>

popWord :: [Char] -> ([Char], [Char])
popWord xs = (fst tuple, strip (snd tuple))
    where tuple = span (/= ' ') (strip xs)

-- Processa uma string e retorna 
-- a lista de suas palavras. OBS: 
-- palavras não devem ter espaços 
-- extemos e nem serem vazias. Exemplo,

-- >> splitStr " The   fox jumped  "
-- ["The", "fox", "jumped"]

splitStr :: [Char] -> [ [Char] ]
splitStr [] = []
splitStr xs = fst tuple : splitStr (strip (snd tuple))
    where tuple = span (/= ' ') (strip xs)