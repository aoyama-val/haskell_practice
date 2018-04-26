module Prob21 where

import Sieve

primeList = Sieve.sieve 10000

sumOfDivisors n = (sum $ Sieve.divisors primeList n) - n

isAmicable n = let counterpart = sumOfDivisors n in n /= counterpart && sumOfDivisors counterpart == n

prob21 = sum $ [n | n <- [2..10000], isAmicable n]
