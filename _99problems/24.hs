prefix :: Eq a => [a] -> [a] -> Bool
prefix _ [] = True
prefix [] _ = False
prefix (x:xs) (y:ys)
    | x == y    = prefix xs ys
    | otherwise = False

suffix :: Eq a => [a] -> [a] -> Bool
suffix xs ys = prefix (reverse xs) (reverse ys)

main = do
    print $ suffix "abcdef" "def"
    print $ suffix "abcdef" ""
    print $ suffix "abcdef" "fg"
