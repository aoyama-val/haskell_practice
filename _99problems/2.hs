isSingle :: [x] -> Bool
isSingle xs = (not (null xs)) && (null (tail xs))

isDouble :: [x] -> Bool
isDouble xs = (not (null xs)) && isSingle(tail(xs))

main = do
    print $ isDouble []
    print $ isDouble [1]
    print $ isDouble [1, 2]
    print $ isDouble [1, 2, 3]
