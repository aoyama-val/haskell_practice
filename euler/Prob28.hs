module Prob28 where

corners n = [n^2, n^2 - (n-1), n^2 - (n-1)*2, n^2 - (n-1)*3]
cornersSum n = sum $ corners n

prob28 = 1 + (sum $ map cornersSum [3, 5..1001])


