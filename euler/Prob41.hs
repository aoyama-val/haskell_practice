--module Prob41 where

import Data.List
import Sieve

--digitsToInt :: [Int] -> Int
--digitsToInt xs = sum $ zipWith (\a -> \n -> a*10^n) (reverse xs) [0..]

isPandigital s = sort s == "1234567"

main = do
    primes <- loadPrimes "primes.txt"
    print $ head [p | p <- reverse primes, isPandigital (show p)]
