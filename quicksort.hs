{-
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = 
    let smallerOrEqual = [ a | a <- xs, a <= x ]
        larger         = [ a | a <- xs, a > x ]
    in  (quicksort smallerOrEqual) ++ [x] ++ (quicksort larger)
-}

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = 
    let le = filter (\a -> a <= x) xs
        gt = filter (\a -> a > x)  xs
    in  (quicksort le) ++ [x] ++ (quicksort gt)


main = do
    print $ quicksort [5,2,4,1,15, 7,3,6, 2]
