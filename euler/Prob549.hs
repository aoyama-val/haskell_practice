import qualified Data.MemoCombinators as Memo
import qualified Data.Numbers.Primes as P
import Data.List
import System.Environment
import Control.Monad

-- factorAsPair 100 = [(2,2),(5,2)]
factorAsPair :: Int -> [(Int, Int)]
factorAsPair n = map (\x -> (head x, length x)) (group (P.primeFactors n))

-- n = p^a のときの s()
s''' :: Int -> Int -> Int
s''' p a =
    let prev = if a <= 2 then p else s (p^(a-1))
    in case find (\x -> logPFactN p x >= a) [(prev),(prev + p)..] of
            Just b -> b
            Nothing -> 0

s'''memo = Memo.integral s'''

-- n! がpの何乗で割り切れるか
logPFactN p n = sum $ map (\x -> n `div` x) $ (takeWhile (<= n)) [p^i | i <- [1..]]

ssum n i acc
    | n < i = acc
    | otherwise = ssum n (i+1) (acc + s i)

s ::  Int -> Int
s n =
    let pairs = factorAsPair n
    in  if length pairs == 1
            then if (snd $ head pairs) == 1
                    then (fst $ head pairs)
                    else s'''memo (fst $ head pairs) (snd $ head pairs)
        else
            maximum [s'''memo (fst pair) (snd pair) | pair <- pairs]

main = do
    args <- getArgs
    let n = read (head args) :: Int
    print $ ssum (10^n) 2 0
    --mapM_ (putStrLn . show . s) [2..10^n]
