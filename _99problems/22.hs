mergeList :: Ord a => [a] -> [a] -> [a]
mergeList xs [] = xs
mergeList [] ys = ys
mergeList (x:xs) (y:ys)
    | x < y     = x : mergeList xs (y:ys)
    | x == y    = x : y : mergeList xs ys
    | otherwise = y : mergeList (x:xs) ys

mergeSort [] = []
mergeSort (x:xs) = mergeList [x] (mergeSort xs)

main = print $ mergeSort [5, 6, 4, 7, 8, 3, 2, 9, 1, 10]
