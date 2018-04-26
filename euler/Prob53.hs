fac n = fac' n 1
    where
        fac' 1 a = a
        fac' n a = fac' (n-1) n * a

c n r = (fac n) `quot` (fac r) `quot` (fac (n-r))

allGT1000000 n = 
    let halfN = n `quot` 2
    in  [c n r | r <- [(n-1),(n-2)..1], c n r > 1000000]


prob53 = sum $ map (length . allGT1000000) [1..100]

main = do
    print prob53
