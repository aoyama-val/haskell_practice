--isInteresting n step = [n, n + step, n + step + step]

import Data.List

import Sieve
import qualified Data.Set as Set

isInterestingSequence primes xs = (all (isPrime primes) xs) && (length $ nub $ map (sort . show) xs) == 1

isPrime primes n = n `elem` primes

main = do
    primeList <- loadPrimes "primes_under_1000000.txt"
    let primes = Set.fromList primeList
    --print $ isPrime primes 11
    --print $ isInterestingSequence primes [ 1487, 4817, 8147]
    print $ [(start, n2, n3) | start <- primeList, start >= 1000, start < 9999, step <- [2,4..4000], let n2 = start+step, let n3 = n2+step, n3 <= 9999, isInterestingSequence primes [start, n2, n3]]
    --print $ length [start | start <- primeList, start >= 1000, start < 9999]

