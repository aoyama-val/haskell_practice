intersection :: Eq a => [a] -> [a] -> [a]
intersection [] _ = []
intersection (x:xs) ys
    | elem x ys = x : intersection xs ys
    | otherwise = intersection xs ys

main = do
    print $ intersection "abcd" "cdef"
