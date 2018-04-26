subseq :: [t] -> Int -> Int -> [t]
subseq xs n m
    | n == 0    = take m xs
    | n < m     = subseq (tail xs) (n - 1) (m - 1)
    | otherwise = []

lis = ["a", "b", "c", "d", "e"]

main = do
    print $ subseq lis 2 4
    print $ subseq lis 0 5
    print $ subseq lis 0 0
