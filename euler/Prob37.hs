module Prob37 where

import qualified Data.Set       as Set

import Sieve

isPrime primes n = n `elem` primes

-- Data.List.tails がほぼ同じ（ただし空配列も含む）
truncates :: [a] -> [[a]]
truncates [] = []
truncates xs = xs : (truncates $ tail xs)

truncateFromLeft :: Int -> [Int]
--truncateFromLeft _ = []
truncateFromLeft n =
    let s = show n
        t = truncates (tail s)
    in  map (read :: String -> Int) t

truncateFromRight :: Int -> [Int]
truncateFromRight n =
    let s = reverse $ show n
        t = truncates (tail s)
    in  map ((read :: String -> Int) . reverse) t

bothTruncatable :: Set.Set Int -> Int -> Bool
bothTruncatable primes n = all (isPrime primes) (truncateFromLeft n) && all (isPrime primes) (truncateFromRight n)

prob37 = do
    primes <- loadPrimes "primes_under_1000000.txt"
    let primeSet = Set.fromList primes
    return $ [ p | p <- primes, bothTruncatable primeSet p ]

main = do
    result <- prob37
    print $ result
