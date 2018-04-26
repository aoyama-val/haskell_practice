prefix :: Eq a => [a] -> [a] -> Bool
prefix _ [] = True
prefix [] _ = False
prefix (x:xs) (y:ys)
    | x == y    = prefix xs ys
    | otherwise = False

main = do
    print $ prefix "abcdef" "abc"
    print $ prefix "abcdef" "abce"
    print $ prefix "abcdef" ""
