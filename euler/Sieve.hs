-- Prob3.hsで実装したエラトステネスのふるいは遅いので、高速化したバージョン
--
-- しかし実は試し割りでやった方が速いらしい
-- https://projecteuler.net/thread=10;page=2
--
-- こちらのData.Setをつかうふるいも試し割りと同じくらいの速さ
-- http://na-o-s.hateblo.jp/entry/2014/11/06/205433

module Sieve where

import Data.List
import qualified Data.Set as Set

and2 :: Bool -> Bool -> Bool
and2 x y = and [x, y]

sieve n = let booleans = _sieve [2..n] (replicate (n - 1) True) 2 (floor (sqrt (fromIntegral n))) (n - 1)
           in   [x | (x, y) <- (zip [2..n] booleans), y]

_sieve :: [Int] -> [Bool] -> Int -> Int -> Int -> [Bool]
_sieve all result x sqrtOfN len = 
    let isXPrime = result !! (x - 2)
    in
        if isXPrime then
            if x <= sqrtOfN then
                let result' = zipWith and2 result ((replicate (x - 1) True) ++ (cycle ((replicate (x - 1) True) ++ [False])))
                in  _sieve all result' (x + 1) sqrtOfN len
            else
                result
        else
            _sieve all result (x + 1) sqrtOfN len

-- 素因数分解
-- factor 10 = [2,2,5,5]
factor :: [Int] -> Int -> [Int]
factor primeList n = 
    let a = find (\x -> n `mod` x == 0) primeList in
    case a of
        Just p -> p : factor primeList (n `div` p)
        Nothing -> []

-- 素因数分解
-- factorAsPair 100 = [(2,2),(5,2)]
factorAsPair primeList n = map (\x -> (head x, length x)) (group (factor primeList n))

crossProduct2 a b = [x * y | x <- a, y <- b]

crossProductN :: [[Int]] -> [Int]
crossProductN = foldl crossProduct2 [1]

-- 全ての約数
divisors :: [Int] -> Int -> [Int]
divisors primeList n = let  pairs = factorAsPair primeList n
                            tmp = map (\pair -> map (\power -> (fst pair)^power) [0..(snd pair)]) pairs
                        in crossProductN tmp


-- 一意な素因数のリスト
-- primeFactors 10 = [2,5]
primeFactors :: [Int] -> Int -> [Int]
primeFactors primeList n = nub (factor primeList n)

-- ファイルから素数表を読み込む
loadPrimes :: String -> IO [Int]
loadPrimes filename = do
    str <- readFile filename
    let primes = map (read :: String -> Int) (lines str)
    return $ primes

loadPrimesAsSet :: String -> IO (Set.Set Int)
loadPrimesAsSet filename = do
    primes <- loadPrimes filename
    return $ Set.fromList primes

primeListToSet :: [Int] -> Set.Set Int
primeListToSet primeList = Set.fromList primeList
