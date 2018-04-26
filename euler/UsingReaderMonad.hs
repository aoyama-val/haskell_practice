-- Reader Monadを使って素数表をグローバル変数化するサンプル

module UsingReaderMonad where

import Control.Monad.Reader

sieve [] = []
sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]

primes = take 100 (sieve [2..])

hoge :: Int -> Reader [Int] [Int]
hoge x = do
    primes <- ask
    return [p | p <- take 10 primes, x `mod` p == 0]

moge :: Int -> Reader [Int] [Int]
moge x = do
    primes <- ask
    mojira <- hoge 120
    return $ x : mojira

main = print $ runReader (moge 30) primes
