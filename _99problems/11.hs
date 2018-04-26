module Count where
{-| 数える

>>> count 'a' "ababcabcd"
2

-}
count :: Eq a => a -> [a] -> Int
count x [] = 0
count x (y:ys) = 
    if x == y then
        1 + (count x ys)
    else
        count x ys

main = do
    print $ count 'a' "ababcabcd"
    print $ count 'c' "ababcabcd"
    print $ count 'd' "ababcabcd"
    print $ count 'e' "ababcabcd"
