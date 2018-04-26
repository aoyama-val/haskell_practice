import Data.List

ts = [n*(n+1) `quot` 2 | n <- [1..]]

ps = [n*(3*n-1) `quot` 2 | n <- [1..]]

hs = [n*(2*n-1) | n <- [1..]]

isInteger :: Double -> Bool
isInteger x = (fromIntegral (floor x)) - x == 0

isT :: Double -> Bool
isT x = let a = ((-1) + sqrt (1 + 8*x)) / 2
        in  isInteger a

isP :: Double -> Bool
isP x = isInteger $ (1 + sqrt (1 + 24*x)) / 6

isH :: Double -> Bool
isH x = isInteger $ (1 + sqrt (1 + 8*x)) / 4

main = do
    print $ take 3 [floor n | n <- hs, isT n, isP n]

    -- 別解
    --let n = 90000
    --print $ take 3 ((take n ts) `intersect` (take n ps) `intersect` (take n hs))

