compress :: Eq a => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x:y:ys) = if x == y then compress (y:ys) else x:(compress (y:ys))
main = do
    print $ compress "aaaabccaadeeee"
