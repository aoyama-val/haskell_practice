isAdjacent :: Eq a => a -> a -> [a] -> Bool
isAdjacent x y [] = False
isAdjacent x y (z:zs)
    | null zs                   = False
    | x == z && y == (head zs)  = True
    | otherwise                 = isAdjacent x y zs

main = do
    print $ isAdjacent 'a' 'b' "abcdef"
    print $ isAdjacent 'e' 'f' "abcdef"
    print $ isAdjacent 'f' 'e' "abcdef"
