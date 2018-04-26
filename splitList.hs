splitList xs
    | null xs       = []
    | length xs < 5 = [xs]
    | otherwise     = [take 5 xs] ++ (splitList (drop 5 xs))

-- length xs < 5の行は実際は不要

main = print(splitList (take 31 [1,2..]))
