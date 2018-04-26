--module Prob23 where

import Sieve hiding (main)
import Data.List
import qualified Data.Set as Set


--prob23 :: Int
prob23 =
    let primes = sieve 28123
        abundants = [n | n <- [2..28123] :: [Int], isAbundant primes n]
        nums = [ (a + b) | a <- abundants, b <- abundants, a <= 14062, a <= b]
    in foldl (+) 0 $ (Set.fromList [1..28123]) `Set.difference` (Set.fromList nums)

    -- このようにListのままで \\ を使うと非常に遅い
    --in sum $ ([2..28123] \\ nums)

main = do
    print $ prob23

sumOfProperDivisors primes n = (sum $ Sieve.divisors primes n) - n

isAbundant primes n = (sumOfProperDivisors primes n) > n
