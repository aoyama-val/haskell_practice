butLastN [] _ = []
butLastN xs n
    | (length xs) > n  = (head xs) : (butLastN (tail xs) n)
    | otherwise        = []

lis = ["a", "b", "c", "d", "e"]

main = do
    print $ butLastN lis 3
    print $ butLastN lis 0
    print $ butLastN lis 5
