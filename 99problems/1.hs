myLast :: [a] -> a
--myLast [] =  error "empty list"
--myLast [a] = a
--myLast (x:xs) = myLast xs
myLast = foldl1 (\acc x -> x)

main = do
    print $ myLast [1,2,3,4]
    print $ myLast ['x', 'y', 'z']
