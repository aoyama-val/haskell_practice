import qualified Data.List.Split as Split
import Data.List

isLeap n = n `mod` 4 == 0 && (n `mod` 100 /= 0 || n `mod` 400 == 0)

oy = 2017
om = 1
od = 1

dayCount y
    | isLeap y  = 366
    | otherwise = 365

daysInMonth y m
    | m <= 0    = 0
    | otherwise = (daysList y) !! (m-1)

daysList y
    | isLeap y  = [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    | otherwise = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

dayCountUntilPrevYear y = sum $ map dayCount [oy..(y-1)]

dayCountSinceBeginningOfYear y m d  = (sum $ map (daysInMonth y) [0..(m-1)]) + d - 1

dayCountSinceOrigin y m d = (dayCountUntilPrevYear y) + dayCountSinceBeginningOfYear y m d

wday y m d = dayCountSinceOrigin y m d `mod` 7 

prevMonth y m
    | m == 1    = (y-1, 12)
    | otherwise = (y, m-1)

calendar :: Int -> Int -> [[Int]]
calendar y m =
    let wd = wday y m 1
        prev = prevMonth y m
    --in  (takeLast wd [1..(daysInMonth (fst prev) (snd prev))]) ++ [1..(daysInMonth y m)]
        lis = (takeLast wd [1..(daysInMonth (fst prev) (snd prev))]) ++ [1..(daysInMonth y m)]
    in Split.chunksOf 7 (lis ++ (take (7 - length lis `mod` 7) [1..]))

takeLast n xs = reverse $ take n $ reverse xs

format n
    | n < 10 = " " ++ (show n)
    | otherwise = show n

main = do
    --print $ wday 2017 2 26
    --print $ dayCountSinceOrigin 2018 1 1 
    --print $ dayCountSinceBeginningOfYear 2017 2 1 
    --print $ sum $ daysList 2016
    mapM putStrLn $ map (intercalate " " . map format) $ calendar 2017 5

