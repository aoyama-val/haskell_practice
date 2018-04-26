-- ghciでの実行方法
-- >>> :l Prob3 Prob5
-- [1 of 2] Compiling Prob3            ( Prob3.hs, interpreted )
-- [2 of 2] Compiling Prob5            ( Prob5.hs, interpreted )
-- Ok, modules loaded: Prob3, Prob5.
-- >>> :show modules
-- Prob3            ( Prob3.hs, interpreted )
-- Prob5            ( Prob5.hs, interpreted )
-- >>> :m +Prob5
-- >>> prob5
-- 232792560

module Prob5 where
import Data.List
import Prob3

factorAsPair n = map (\x -> (head x, length x)) (group (factor n))

aux n = head [x | x <- [1..], all (\m -> x `mod` m == 0) [1..n]]

unfactorPairs :: [(Int, Int)] -> Int
unfactorPairs [] = 1
unfactorPairs (x:xs) = (fst x) ^ (snd x) * unfactorPairs xs

prob5 = unfactorPairs $ map (\pairs -> (fst (head pairs), maximum (map snd pairs))) $ groupBy (\x -> \y -> (fst x) == (fst y)) $ sortOn fst $ [2..20] >>= factorAsPair
