myTake [] _ = []
myTake (x:xs) 0 = []
myTake (x:xs) n = x : myTake xs (n - 1)

main = do
    print $ myTake ["a", "b", "c", "d", "e"] 3
    print $ myTake ["a", "b", "c", "d", "e"] 0
    print $ myTake ["a", "b", "c", "d", "e"] 6
