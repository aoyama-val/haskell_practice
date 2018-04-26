module Prob1 where

prob1 :: Int -> Int
prob1 n = sum [x | x <- [1..(n-1)], x `mod` 3 == 0 || x `mod` 5 == 0]
