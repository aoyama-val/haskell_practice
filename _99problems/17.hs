--ogosh> (set-of-list '(a b c d e f a b c))
--(d e f a b c)

setOfList :: Eq a => [a] -> [a]
setOfList [] = []
setOfList (x:xs)
    | elem x xs = setOfList xs
    | otherwise = x : (setOfList xs)

main = do
    print $ setOfList "abcdefabc"
