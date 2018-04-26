myButLast :: [a] -> a
--myButLast [] = error "empty list"
--myButLast [x] = error "empty list"
--myButLast [x,y]= x
--myButLast (x:xs) = myButLast xs
myButLast = head . (drop 1) . reverse

main = do
    print $ myButLast [1,2,3,4]
    print $ myButLast ['a'..'z']
