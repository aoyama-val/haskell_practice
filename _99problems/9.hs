group [] _ = []
group xs n
    | (length xs) >= n  = [take n xs] ++ (group (drop n xs) n)
    | otherwise         = [xs]

charToString :: Char -> String
charToString c = [c]

lis = map charToString ['a'..'f']

main = do
    print $ group lis 2
    print $ group lis 3
    print $ group lis 4
