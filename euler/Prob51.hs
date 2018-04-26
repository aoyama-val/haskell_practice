--module Prob51 where

import qualified Data.Numbers.Primes as P
import qualified Data.MemoCombinators as Memo
import qualified Data.Char as Char
import Data.List
import Control.Monad

comb :: Int -> [a] -> [[a]]
comb 0 xs = [[]]
comb _ [] = []
comb n (x:xs) = [x:y | y <- comb (n-1) xs] 
        ++ comb n xs

intToDigits :: Int -> [Int]
intToDigits n = map ((subtract 48) . Char.ord) (show n)

digitsToInt :: [Int] -> Int
digitsToInt xs = sum $ zipWith (\a -> \n -> a*10^n) (reverse xs) [0..]

replace num comb n =
    let digits = intToDigits num
    in  [if i `elem` comb then n else (digits !! i) | i <- [0..(length digits - 1)]]

replace0to9 num comb =
    if head comb == 0
        then [digitsToInt $ replace num comb x | x <- [1..9]]
        else [digitsToInt $ replace num comb x | x <- [0..9]]

powerset :: [a] -> [[a]]  
powerset xs = filterM (\x -> [True, False]) xs

primeCount xs = length $ filter P.isPrime xs

maskPoints digitCount = filter (\xs -> (length xs >= 2)) (powerset [0..(digitCount - 2)])

allReplaced p = [replace0to9 p mp | mp <- (maskPoints (length $ show p))]

isEightFamiliy p = any (\xs -> primeCount xs == 8) (allReplaced p)

prob51 = case find isEightFamiliy P.primes of
            Just p -> head $ filter P.isPrime $ head $ filter (\xs -> primeCount xs == 8) (allReplaced p)
            Nothing -> 0

main = do
    print prob51
