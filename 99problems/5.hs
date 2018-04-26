myReverse :: [a] -> [a]
--myReverse [] = []
--myReverse (x:xs) = (myReverse xs) ++ [x]
myReverse = foldl (\acc x -> x:acc) []

main = do
    print $ myReverse "A man, a plan, a canal, panama!"
