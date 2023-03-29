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