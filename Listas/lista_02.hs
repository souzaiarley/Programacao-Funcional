paridade :: [Bool] -> Bool
paridade lista
    | mod (length [x | x <- lista, x == True]) 2 == 1 = True
    | otherwise = False

rev :: Int -> Int
rev n = read (reverse(show n))

delete' :: (Eq a) => a -> [a] -> [a]
delete' x lista = fst (span (/= x) lista) ++ tail (snd  (span (/= x) lista))  

swap :: [a] -> Int -> Int -> [a]
swap lst p q
    | p < 0 || q < 0 || p >= length lst || q >= length lst = error "Posições inválidas"
    | otherwise = [if i == p then lst !! q else if i == q then lst !! p else x | (x, i) <- zip lst [0..]]


maxsseq :: (Ord a, Num a) => [a] -> [a]
maxsseq [] = []
maxsseq [x] = [x]
maxsseq (x:xs) = if sum (x:xs) > sum maxTail
                    then (x:xs)
                    else maxTail
    where maxTail = maxsseq xs

listacc :: Num a => [a] -> [a]
listacc [] = []
listacc lst = scanl1 (+) lst

buscabin :: (Ord a) => [a] -> a -> Int
buscabin lst x = case dropWhile (\(_, v) -> v /= x) (zip [0..] lst) of
                   [] -> -1
                   ((i, ):) -> i