import Data.List

pentagonals :: [Int]
pentagonals = [n * (3*n-1) `quot` 2 | n <- [1..]]

isPentagonal n = (head $ dropWhile (< n) pentagonals) == n

--f = [(x, y) | x <- pentagonals, y <- pentagonals, isPentagonal (x+y)]

--f = [(x, y) | j <- [0..], let k = j + 3, let x = pentagonals !! j, let y = pentagonals !! k, isPentagonal (x+y), isPentagonal (y-x)]

pentagonalsFromWithin x d = takeWhile (<= x + d) (dropWhile (< x) pentagonals)

--f d = [ [ | pentagonalsFromWithin x j | j <- [0..], let x = pentagonals !! j]

comb :: Int -> [a] -> [[a]]
comb 0 xs = [[]]
comb _ [] = []
comb n (x:xs) = [x:y | y <- comb (n-1) xs] 
        ++ comb n xs


main = do
    --print $ take 30 pentagonals
    --print $ map isPentagonal [1..25]
    --print $ pentagonalsFromWithin 22 90
    print $ [(x, y) | c <- comb 2 (take 3000 pentagonals), let x = c !! 0, let y = c !! 1, isPentagonal (x + y), isPentagonal (y - x)]
    --print $ pentagonals !! 1999
    --print $ find (\d -> not (null [pj | pj <- pentagonals, isPentagonal (pj + d), isPentagonal (pj + pj + d)])) pentagonals
    --pentagonalsFromWithin d| d <- pentagonals
