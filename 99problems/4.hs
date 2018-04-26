myLength :: [a] -> Int
myLength = foldr (\x acc ->acc + 1)  0

main = do
    print $ myLength [123, 456, 789]
    print $ myLength "Hello, world!"
