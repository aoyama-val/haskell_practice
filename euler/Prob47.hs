--module Prob47 where

import Sieve
import Data.Foldable

hasNDistinctPrimes :: [Int] -> Int -> Int -> Bool
hasNDistinctPrimes primeList n x = length (primeFactors primeList x) == n

prob47 :: Maybe Int
prob47 = find (\x -> (hasNDistinctPrimes thePrimeList conseq x) && (hasNDistinctPrimes thePrimeList conseq (x+1)) && (hasNDistinctPrimes thePrimeList conseq (x+2)) && (hasNDistinctPrimes thePrimeList conseq (x+3))) [2..10000000]
        where
            thePrimeList = sieve 1000000
            conseq = 4

main = print $ prob47
