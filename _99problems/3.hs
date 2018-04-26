isLonger xs ys 
    | (null xs)       && (null ys)          = False
    | (null xs)       && (not $ null ys)    = False
    | (not $ null xs) && (null ys)          = True
    | otherwise                             = isLonger (tail xs) (tail ys)

main = do
    print $ isLonger [1,2,3] [1,2]
    print $ isLonger [1,2] [1,2]
    print $ isLonger [1,2] [1,2,3]
