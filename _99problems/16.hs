iota :: (Num a, Ord a) => a -> a-> [a]
iota x y
    | x <= y     = x : iota (x + 1) y
    | otherwise = []

main = do
    print $ iota 1 5 
    print $ iota 4 8
    print $ iota 10 8
