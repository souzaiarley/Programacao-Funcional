crop :: [a] -> [a]
crop l
    | length l == 0 || length l == 1 = []
    | otherwise = init (tail l)