isBefore :: Eq a => a -> a -> [a] -> Bool
isBefore x y [] = False
isBefore x y (z:zs)
    | null zs                   = False
    | x == z                    = elem y zs
    | otherwise                 = isBefore x y zs

main = do
    print $ isBefore 'a' 'b' "abcdef"
    print $ isBefore 'c' 'b' "abcdef"
