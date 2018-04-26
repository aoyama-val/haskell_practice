split :: String -> [String]
split "" = []
split s
    | (head s) == ',' = [""] ++ split (tail s)
    | otherwise       = 
        case split (tail s) of
            [] -> [[head s]]
            result -> ((head s) : (head result)) : (tail result)

main = do
    print $ split ""
    print $ split ","
    print $ split "moge"
    print $ split "moge,sage"
    print $ split "  moge  , sage, "
    print $ split "hage, moge, sage"
