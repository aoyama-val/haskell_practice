difference :: Eq a => [a] -> [a] -> [a]
difference [] ys = []
difference (x:xs) ys
    | elem x ys = difference xs ys
    | otherwise = x : difference xs ys

main :: IO ()
main = print $ difference "abcd" "cdef"
