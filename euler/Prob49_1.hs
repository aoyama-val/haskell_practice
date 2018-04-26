-- 別解。こちらの方が速い
-- １．素数表から4桁の素数を抽出する
-- ２．各素数を構成する数字でグループ化し、3個以上の要素からなるグループを抽出
-- ３．各グループに対し、順列を生成し、全て素数かつ等差数列であるか判定する

import Data.List
import qualified Data.Set as Set
import qualified Data.Function as Function

import Sieve

candidates contents = map (\xs -> map ((read :: String -> Int) . fst) xs) $ filter ((>= 3) .  length) $ groupBy (\a -> \b -> (snd a) == (snd b)) $ sortOn snd $ map (\str -> (str, sort str)) (lines contents)

comb :: Int -> [a] -> [[a]]
comb 0 xs = [[]]
comb _ [] = []
comb n (x:xs) = [x:y | y <- comb (n-1) xs] 
        ++ comb n xs

isInterestingSequence primes xs = (all (isPrime primes) xs) && (length $ nub $ map (sort . show) xs) == 1 && (xs !! 2) - (xs !! 1) == (xs !! 1) - (xs !! 0)

isPrime primes n = n `elem` primes

main = do
    primeList <- loadPrimes "4digits_primes.txt"
    let primes = Set.fromList primeList
    contents <- readFile "4digits_primes.txt"
    print [numbers | cand <- candidates contents, c <- comb 3 cand, let numbers = c, isInterestingSequence primes numbers]
