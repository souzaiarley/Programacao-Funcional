import Data.Char

menorDeDois :: Int -> Int -> Int
menorDeDois a b = if a <= b then a else b

menorDeTres :: Int -> Int -> Int -> Int
menorDeTres a b c
    | a <= b && a <= c = a
    | b <= a && b <= c = b
    | otherwise = c

fatorial :: Int -> Int
fatorial n
    | n == 0 = 1
    | otherwise = n * fatorial (n-1)


fibonacci :: Int -> Int
fibonacci n
    | n == 0 = 0
    | n == 1 = 1
    | otherwise = fibonacci (n-1) + fibonacci(n-2)

elemento :: [Int] -> Int -> Int
elemento [] _ = -1
elemento a n = a !! n

pertence :: [Int] -> Int -> Bool
pertence [] _ = False
pertence (x:xs) n
    | x == n = True
    | otherwise = pertence xs n

total :: [Int] -> Int
total [] = 0
total (x:xs) = 1 + total xs

maior :: [Int] -> Int
maior [] = -1
maior (x:xs)
    | x >= maior xs = x
    | otherwise = maior xs

frequencia :: Int -> [Int] -> Int
frequencia _ [] = -1
frequencia n lista = length [x | x <- lista, x == n]

unico :: Int -> [Int] -> Bool
unico _ [] = False
unico n lista
    | length [x | x <- lista, x == n] == 1 = True
    | otherwise = False

maioresQue :: Int -> [Int] -> [Int]
maioresQue n lista = [x | x <- lista, x > n]

concate :: [Int] -> [Int] -> [Int]
concate a b = a++b

calda :: [Int] -> [Int]
calda [] = []
calda (x:xs) = xs

corpo :: [Int] -> [Int]
corpo x = init x;

unique :: [Int] -> [Int]
unique [] = []
unique (x:xs) = x : [n | n <- unique xs, n /= x]

menores :: Int -> [Int] -> [Int]
menores _ [] = []
menores n lista = [x | x <- lista, x <= n]

alter :: Int -> [Int]
alter 0 = []
alter n = alter (n-1) ++ [n] ++ [(-n)]

reverso :: [Int] -> [Int]
reverso [] = []
reverso (x:xs) = reverso xs ++ [x]

divide :: [Int] -> Int -> ([Int], [Int])
divide lista n = (take n lista, drop n lista)

intercal :: [Int] -> [Int] -> [Int]
intercal [] [] = []
intercal lista [] = lista
intercal [] lista = lista
intercal (x:xs) (y:ys) = x : y : intercal xs ys

uniao :: [Int] -> [Int] -> [Int]
uniao a b = unique a ++ b

intersec :: [Int] -> [Int] -> [Int]
intersec a b = unique [n | n <- a++b, unico n (a++b) == False]

sequencia :: Int -> Int -> [Int]
sequencia n m = [m..m+(n-1)]

inserir :: Int -> [Int] -> [Int]
inserir n (x:xs)
    | n < x = n : (x:xs)
    | otherwise = x : inserir n xs

isSorted :: [Int] -> Bool
isSorted a
    | length a == 1 = True
isSorted (x:xs)
    | length xs > 0 && x <= head xs = isSorted xs
    | otherwise = False

qsort :: [Int] -> [Int]
qsort [] = []
qsort lista = qsort[n | n <- lista, n < head lista] ++ [head lista] ++ qsort [n | n <- lista, n > head lista]

rotEsq :: Int -> [Char] -> [Char]
rotEsq _ [] = []
rotEsq 0 lista = lista
rotEsq n lista = rotEsq (n-1) newlista
    where newlista = tail lista ++ [head lista]

rotDir :: Int -> [Char] -> [Char]
rotDir _ [] = []
rotDir 0 lista = lista
rotDir n lista = rotDir (n-1) newlista
    where newlista = [last lista] ++ init lista

upper :: [Char] -> [Char]
upper [] = []
upper (x:xs)
    | x >= 'a' && x <= 'z' = chr ((ord x)-32) : upper xs
    | otherwise = x : upper xs

titulo :: [Char] -> [Char]
titulo [] = []
titulo string
    | snd tupla /= "" = toUpper (head (fst tupla)) : map toLower (tail (fst tupla)) ++ " " ++ titulo (tail (snd tupla))
    | otherwise = toUpper (head (fst tupla)) : map toLower (tail (fst tupla))
    where tupla = span (/= ' ') string

selec :: [a] -> [Int] -> [a]
selec [] _ = []
selec _ [] = []
selec list (x:xs) = (list !! x) : selec list xs

isPalind :: [Char] -> Bool
isPalind [] = True
isPalind [_] = True
isPalind str
    | str == reverse str = True
    | otherwise = False

primo :: Int -> Bool
primo x = length ([n | n <- [2..x], mod x n == 0]) == 1

sdig :: Int -> Int
sdig 0 = 0
sdig n = (mod n 10) + (sdig (div n 10))

bubblesort :: Ord a => [a] -> [a]
bubblesort s = case bsort' s of
               t | t == s    -> t
                 | otherwise -> bubblesort t
  where bsort' (x:x2:xs) | x > x2    = x2:(bsort' (x:xs))
                         | otherwise = x:(bsort' (x2:xs))
        bsort' s = s

compac :: [Int] -> [[Int]]
compac [] = []
compac (x:xs)
    | size > 1 = [[size, x]] ++ compac (snd (span(==x) (x:xs)))
    | otherwise = [[x]] ++ compac (snd (span(==x) (x:xs)))
    where size = length (fst (span(==x) (x:xs)))

splitints :: [Int] -> ([Int], [Int])
splitints [] = ([], [])
splitints lista = (impares, pares)
    where
        impares = [x | x <- lista, mod x 2 == 1]
        pares = [x | x <- lista, mod x 2 == 0]

perfeito :: Int -> Bool
perfeito n
    | length [x | x <- [1..n], x*x == n] == 1 = True
    | otherwise = False

base :: Int-> Int -> [Char]
base 0 _ = []
base n b =  base (div n b) b ++ last
    where last 
            | mod n b < 10 = show (mod n b)
            | otherwise = [chr ((mod n b) - 10 + ord 'A')]