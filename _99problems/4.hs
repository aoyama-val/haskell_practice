myLast [x] = x
myLast (x:xs) = myLast xs

butLast [x] = []
butLast (x:xs) = x : (butLast xs)

main = do
    print $ myLast ["a"]
    print $ myLast ["a", "b", "c"]
    print $ butLast ["a"]
    print $ butLast ["a", "b", "c"]
