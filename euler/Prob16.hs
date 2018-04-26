module Prob16 where

logn a b = (log b) / (log a)

prob16 = map (\x -> read (x:"")::Int) (show $ 2^1000)
