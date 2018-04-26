--module Prob14 where

import Data.List (maximumBy)
import Data.Function (on)

collatz :: Int -> [Int]
collatz n = n : collatz (if even n then n `div` 2 else  3 * n + 1)

collatzLength n = 1 + (length $ takeWhile (/= 1) $ collatz n)

main = print $ maximumBy (compare `on` collatzLength) [2..1000000]
