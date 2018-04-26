module Prob3 where

import Data.List

sieve :: Int -> [Int]
sieve n = _sieve [2..n] (floor $ fromIntegral n)

_sieve :: [Int] -> Int -> [Int]
_sieve [] _ = []
_sieve all@(x:xs) sqrtOfN =
    if x <= sqrtOfN then
        x : _sieve [y | y <- xs, y `mod` x /= 0] sqrtOfN
    else
        all

-- 末尾再帰最適化したバージョン。なぜかこちらの方が遅い
sieve' n = _sieve' [2..n] []
_sieve' :: [Int] -> [Int] -> [Int]
_sieve' [] result = result
_sieve' (x:xs) result = _sieve' [y | y <- xs, y `mod` x /= 0] (x : result)

factor :: Int -> [Int]
factor n = 
    let a = find (\x -> n `mod` x == 0) (sieve n) in
    case a of
        Just p -> p : factor (n `div` p)
        Nothing -> []

prob3 = factor 600851475143
