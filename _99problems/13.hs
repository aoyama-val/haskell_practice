maxList :: Ord a => [a] -> Maybe a
maxList [] =  Nothing
maxList (x:xs) = 
    case maxList xs of
        Nothing -> Just x
        Just n -> 
            if x > n then
                Just x
            else
                Just n


minList :: Ord a => [a] -> Maybe a
minList [] =  Nothing
minList (x:xs) = 
    case minList xs of
        Nothing -> Just x
        Just n -> 
            if x < n then
                Just x
            else
                Just n

main = do
    print $ maxList [5,6,4,7,3,8,2,9,1]
    print $ minList [5,6,4,7,3,8,2,9,1]
