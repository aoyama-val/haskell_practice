module Prob35 where

import qualified Data.Char      as Char
import qualified Data.Set       as Set

import Sieve

isPrime primes n = n `elem` primes

intToDigits :: Int -> [Int]
intToDigits n = map ((subtract 48) . Char.ord) (show n)

digitsToInt :: [Int] -> Int
digitsToInt xs = sum $ zipWith (\a -> \n -> a*10^n) (reverse xs) [0..]

rotate' xs i = (drop i xs) ++ (take i xs)

rotate :: [Int] -> [[Int]]
rotate xs = map (rotate' xs) [0..(length xs - 1)]

rotateNumber :: Int -> [Int]
rotateNumber n = map digitsToInt (rotate (intToDigits n))

isAllRotatedNumbersPrime :: (Set.Set Int) -> Int -> Bool
isAllRotatedNumbersPrime primes p = all (isPrime primes) (rotateNumber p)

prob35 = do
    primes <- loadPrimes "primes_under_1000000.txt"
    let primeSet = Set.fromList primes
    print $ length [ p | p <- primes, isAllRotatedNumbersPrime primeSet p ]

main = prob35
