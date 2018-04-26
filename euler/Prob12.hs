module Prob12 where

import Data.List
import Sieve

triangleNumbers :: [Int]
triangleNumbers = zipWith (+) [1..] (0 : triangleNumbers)

primeList = sieve 100000

prob12 = find (\x -> (length (divisors primeList x)) > 500) triangleNumbers
