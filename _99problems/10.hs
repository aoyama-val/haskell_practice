position :: Eq a => a -> [a] -> Maybe Int
position x [] = Nothing
position x (y:ys)
    | x == y        = Just 0
    | otherwise     = case (position x ys) of
                        Just i  -> Just (i + 1)
                        Nothing -> Nothing

lis = "hoge"

main = do
    print $ position 'h' lis
    print $ position 'o' lis
    print $ position 'g' lis
    print $ position 'e' lis
    print $ position 'a' lis
