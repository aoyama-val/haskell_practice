isSingle :: [x] -> Bool
isSingle xs = (not (null xs)) && (null (tail xs))

main = do
    print $ isSingle []
    print $ isSingle [1]
    print $ isSingle [1, 2]
