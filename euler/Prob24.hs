module Prob24 where

permutation [] = [[]]
permutation xs = xs >>= (\x -> map (x :) (permutation (filter (/= x) xs)))

prob24 = (permutation [0..9]) !! (1000000-1)
