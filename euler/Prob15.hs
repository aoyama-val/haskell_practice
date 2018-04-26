module Prob15 where

fac n = product [1..n]

choose n k = (fac n) `div` (fac k) `div` (fac (n - k))

solve15 n = choose (2*n) n

prob15 = solve15 20
