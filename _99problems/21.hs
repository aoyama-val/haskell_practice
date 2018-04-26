mergeList :: Ord a => [a] -> [a] -> [a]
mergeList xs [] = xs
mergeList [] ys = ys
mergeList (x:xs) (y:ys)
    | x < y     = x : mergeList xs (y:ys)
    | x == y    = x : y : mergeList xs ys
    | otherwise = y : mergeList (x:xs) ys

main = print $ mergeList [1,3,5,7] [2,4,6,8]
