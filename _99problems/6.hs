myDrop [] _ = []
myDrop xs 0 = xs
myDrop (x:xs) n = myDrop xs (n - 1)

main = do
    print $ myDrop ["a", "b", "c", "d", "e"] 3
    print $ myDrop ["a", "b", "c", "d", "e"] 0
    print $ myDrop ["a", "b", "c", "d", "e"] 6
