prefix :: Eq a => [a] -> [a] -> Bool
prefix _ [] = True
prefix [] _ = False
prefix (x:xs) (y:ys)
    | x == y    = prefix xs ys
    | otherwise = False

sublist :: Eq a => [a] -> [a] -> Bool
sublist [] _ = True
sublist _ [] = False
sublist xs ys = or [prefix ys xs, sublist xs (tail ys)]

main = do
    print $ sublist "cde" "abcdef"
    print $ sublist "de" "abcdef"
    print $ sublist "deg" "abcdef"
    print $ sublist "" "abcdef"
    print $ sublist "def" "abcdef"
    print $ sublist "xabcdef" "abcdef"
