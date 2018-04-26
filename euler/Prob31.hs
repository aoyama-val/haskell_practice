--module Prob31 where

payMethods :: [Int] -> Int -> [[Int]]
payMethods [] _ = []
payMethods coins@(c:cs) n
    | n <= 0 = [[]]
    | n >= c = payMethods cs n ++ (map (c :) (payMethods coins (n - c)))
    | otherwise = payMethods cs n

prob31 = length $ payMethods [1, 2, 5, 10, 20, 50, 100, 200] 200

main = print prob31
