--module Prob27 where

import Data.List
import Data.Set as Set

import Sieve

third (a, b, c) = c

--quadratic :: Integer -> Integer -> Integer
quadratic a b n = n^2 + a * n + b

conseq primes a b = takeWhile (\n -> isPrime primes (quadratic a b n)) [0..1000]

conseqLength primes a b = length $ conseq primes a b

isPrime primes n = n `elem` primes

prob27 = let primes = Set.fromList (sieve 1000000)
         in  maximumBy (\x -> \y -> compare (third x) (third y)) [(a, b, conseqLength primes a b) | a <- [-999..999], b <- 2 : [3,5..1000], isPrime primes b]
main = putStrLn $ (show prob27)
