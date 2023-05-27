paridade :: [Bool] -> Bool
paridade lista
    | mod (length [x | x <- lista, x == True]) 2 == 1 = True
    | otherwise = False

rev :: Int -> Int
rev n = read (reverse(show n))