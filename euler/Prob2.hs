module Prob2 where

fibo :: [Int]
fibo = _fibo 1 1

_fibo :: Int -> Int -> [Int]
_fibo x y = x : (_fibo y (x + y))

prob2 :: Int
prob2 = sum $ takeWhile (<= 4000000) [x | x <- fibo, even x]
