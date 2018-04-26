-- この問題はちゃんと解けなかった
-- （上限をなんとなく9000にした）
-- 素数リストのsliceを取ってsumを計算するのでなく
-- 素数リストの1番目からn番目の合計を計算する関数をメモ化再帰で作って、
-- n番目からm番目の合計を計算する関数を作れば高速化できたかもしれない
--
-- スレッドのPerlのコード prob50.pl によると、リストの長さは最大545, 素数は最大4920でいいらしい

import Sieve

import Data.List
import qualified Data.Function as Function
import qualified Data.Set as Set

subListsOfLength n xs =
    let len = length xs
    in  [take n (drop i xs) | i <- [0..(len - n)], fromIntegral (xs !! i) < (1000000 / (fromIntegral n))]

isPrime primes n = n `elem` primes

slice :: Int -> Int -> [a] -> [a]
slice start len xs = (take len (drop start xs))

--howManyConsucuve primeSet primeList start =
    --length $ takeWhile (\len -> isPrime primeSet (sum (slice start len primeList))) [1..]

--findLists primes primeSet len =
    --[(sum sublist, sublist) | sublist <- (subListsOfLength len (takeWhile (<1000000) primes)), isPrime primeSet (sum sublist)]

-- リストxsでstartからlen個の合計を返す
consequenceSum start len xs = sum $ slice start len xs

-- startから始まるとき、連続する和が素数になる最大の長さを返す
findLongest :: [Int] -> Set.Set Int -> Int -> Int -> Int
findLongest primes primeSet start maxNum =
    case find (\len -> let s = (consequenceSum start len primes) in s < maxNum && isPrime primeSet s) [(length primes - start),(length primes - start - 1)..21] of
        Just len -> len
        Nothing -> 0

f primes primeSet maxNum = [(start, len) | start <- [0..(length primes - 1)], let len = findLongest primes primeSet start maxNum]

g primes primeSet maxNum =
    let f_ = f primes primeSet maxNum
        m =  maximumBy (compare `Function.on` snd) f_
        ps =  slice (fst m) (snd m) primes
    in  (sum ps, ps)

main = do
    primes <- loadPrimes "primes_under_1000000.txt"
    let primeSet = primeListToSet primes
    -- 上限をいくつにすればいいかわからないから9000にした
    print $ g (takeWhile (< 9000)  primes) primeSet 1000000
