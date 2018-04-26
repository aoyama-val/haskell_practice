sumList :: Num a => [a] -> a
sumList [] = 0
sumList (x:xs) = x + (sumList xs)

main = do
    print $ sumList [1,2,3,4,5]
