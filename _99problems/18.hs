union :: Eq a => [a] -> [a] -> [a]
union [] ys = ys
union (x:xs) ys =
    if elem x ys then
        union xs ys
    else
        x : union xs ys

main = do
    print $ union "abcd" "cdef"
