module Prob7 where

sieve :: Int -> [Int]
sieve n = _sieve [2..n] 0 (n - 1)

_sieve :: [Int] -> Int -> Int -> [Int]
_sieve [] _ _ = []
_sieve xs index len =
    if len <= index then
        xs
    else
        let p = (xs !! index)
        in  _sieve [y | y <- xs, y <= p || y `mod` p /= 0] (index + 1) len

prob7 = (sieve 1000000) !! (10001 - 1)
