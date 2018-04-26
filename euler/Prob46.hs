import qualified Data.Set as Set
import Data.List

import Sieve

isInteger :: Double -> Bool
isInteger x = (fromIntegral (floor x)) - x == 0

isPrime primes n = n `elem` primes

composites primes = [n | n <- [3,5..], not (isPrime primes n)]

isGoldBachTrue primes n =
    let primesUnderN = takeWhile (< n) primes
    in  any id [isInteger (sqrt(fromIntegral (n - p) / 2.0)) | p <- primesUnderN]

main = do
    primeList <- loadPrimes "primes_under_1000000.txt"
    let primes = Set.fromList primeList
    --print $ take 10 (composites primes)
    print $ Data.List.find (not . (isGoldBachTrue primeList)) (composites primes)
